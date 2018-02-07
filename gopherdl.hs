import Data.List
import Data.Maybe
import Data.Strings
import Network.URI
import Network.Socket
import Control.Exception
import Control.Monad
import System.IO
import System.Directory (createDirectoryIfMissing, doesPathExist)
import Data.ByteString (hPut)
import System.FilePath.Posix
import qualified System.Environment as Env
import qualified Network.Socket.ByteString as BsNet
import qualified Data.ByteString.Char8 as C
import System.Console.GetOpt (OptDescr(Option),
                              ArgDescr(NoArg, ReqArg),
                              getOpt,
                              ArgOrder(RequireOrder),
                              usageInfo)

sendAll = BsNet.sendAll
type ByteString = C.ByteString

-- type text \t path \t host \t port \r\n
type MenuLine = (Char, ByteString, ByteString, ByteString, ByteString)
-- host path port
type GopherUrl = (String, String, String)

data Flag = 
      Recursive
    | MaxDepth Int
    | SpanHosts
    | Help
    | Clobber
    | OnlyMenus
    | NoMenus
    | AscendParent
    | Delay Float deriving (Show, Eq)

data Remote = 
    RemoteFile GopherUrl
  | RemoteMenu GopherUrl
    deriving (Show, Eq)

data Config = Config
 {  recursive :: Bool
  , maxDepth :: Int
  , spanHosts :: Bool
  , help :: Bool
  , clobber :: Bool
  , onlyMenus :: Bool
  , ascendParent :: Bool
  , delay :: Float
  } deriving (Show)

{---------------------}
{------ Helpers ------}
{---------------------}

_debug = True

debugLog :: Show a => a -> IO a
debugLog a =
  (if _debug
    then (hPutStrLn stderr ("DEBUG: " ++ show a) 
         >> hFlush stderr)
    else return ()) >> return a

recvAll :: Socket -> IO ByteString
recvAll sock = 
  BsNet.recv sock 4096 >>= recvMore
  where 
    recvMore bytes =
      if (C.length bytes) == 0 
        then return bytes 
        else recvAll sock >>= return . C.append bytes 

appendCRLF :: ByteString -> ByteString
appendCRLF bs = C.append bs $ C.pack "\r\n"

addrInfoHints :: AddrInfo
addrInfoHints = defaultHints { addrSocketType = Stream }

lineIsMenu :: MenuLine -> Bool
lineIsMenu (t,_,_,_,_) = t == '1'

remoteToUrl (RemoteFile url) = url
remoteToUrl (RemoteMenu url) = url

isRemoteFile (RemoteFile _) = True
isRemoteFile otherwise  = False

showUsage = 
  putStr $ usageInfo "gopherdl [options] [urls]" optionSpec

sameHost url1 url2 =
  (hostOfUrl url1) == (hostOfUrl url2)

nchrs chr n = 
  concat $ replicate n [chr]

putStrLnIf cond msg =
  if cond then putStrLn msg else return ()

putStrIf cond msg =
  if cond then putStr msg else return ()

fixProto s = 
  let noProto s = (snd (strSplit "://" s)) == "" in
  if (noProto s) then ("gopher://" ++ s) else s

sanitizePath path = 
  let nonEmptyString = (/=) 0 . sLen in
  filter nonEmptyString $ strSplitAll "/" path

hostOfUrl :: GopherUrl -> String
hostOfUrl (host, _, _) = host

parseGopherUrl :: String -> Maybe GopherUrl
parseGopherUrl =
  uriToGopherUrl . parseURI . fixProto

{--------------------------}
{------ Argv Parsing ------}
{--------------------------}

optionSpec = 
  let argMaxDepth depth = (MaxDepth (read depth::Int)) 
      argDelay delay = (Delay (read delay::Float)) 
  in
  [ Option "r" [] (NoArg Recursive) "Enable recursive downloads"
  , Option "l" [] (ReqArg argMaxDepth "n") "Maximum depth in recursive downloads"
  , Option "s" [] (NoArg SpanHosts) "Span hosts on recursive downloads"
  , Option "h" [] (NoArg Help) "Show this help"
  , Option "c" [] (NoArg Clobber) "Enable file clobbering (overwrite existing)"
  , Option "m" [] (NoArg OnlyMenus) "Only download gopher menus"
  , Option "p" [] (NoArg AscendParent) "Allow ascension to the parent directories"
  , Option "w" [] (ReqArg argDelay "secs") "Delay between downloads" ]

isMaxDepth (MaxDepth _) = True
isMaxDepth otherwise = False

isDelay (Delay _) = True
isDelay otherwise = False

findMaxDepth def options =
  case (find isMaxDepth options) of
    Just (MaxDepth d) -> d
    _ -> def

findDelay def options =
  case (find isDelay options) of
    Just (Delay d) -> d
    _ -> def

configFromGetOpt :: ([Flag], [String], [String]) -> ([String], Config)
configFromGetOpt (options, arguments, errors) = 
  ( arguments, 
    Config { recursive = has Recursive
           , maxDepth = findMaxDepth 99 options
           , spanHosts = has SpanHosts
           , help = has Help
           , clobber = has Clobber
           , onlyMenus = has OnlyMenus
           , ascendParent = has AscendParent
           , delay = findDelay 0.0 options })
  where 
    has opt = opt `elem` options 

parseWithGetOpt :: [String] -> ([Flag], [String], [String])
parseWithGetOpt argv = getOpt RequireOrder optionSpec argv

argvToConfig :: [String] -> ([String], Config)
argvToConfig = configFromGetOpt . parseWithGetOpt

uriToGopherUrl :: Maybe URI -> Maybe GopherUrl
uriToGopherUrl Nothing = Nothing
uriToGopherUrl (Just uri) =
  case (uriAuthority uri) of
    Just auth -> Just ((getHost auth), (getPath uri), (getPort auth))
    otherwise -> Nothing
  where 
    (?>>) a def = if a == "" then def else a
    getHost auth = (uriRegName auth)
    getPath uri = (uriPath uri) ?>> "/"
    getPort auth = (strDrop 1 (uriPort auth)) ?>> "70"

{------------------------}
{----- Menu Parsing -----}
{------------------------}

mlToUrl :: MenuLine -> GopherUrl
mlToUrl (_, _, path, host, port) =
  (C.unpack host, C.unpack path, C.unpack port)

urlToString :: GopherUrl -> String
urlToString (host, path, port) =
  host ++ ":" ++ port ++ path

validLine :: MenuLine -> Bool
validLine line = 
  validPath line && validType line
  where 
    validPath (_, _, path, _, _) = 
      sStartsWith path (C.pack "/")
    validType (t, _, _, _, _) = 
      t `notElem` ['7', '2', '3', '8', 'T']

parseMenu :: ByteString -> [MenuLine]
parseMenu rawMenu = 
  let lines = map parseMenuLine $ C.lines rawMenu in
  filter validLine $ catMaybes $ lines

parseMenuLine :: ByteString -> Maybe MenuLine
parseMenuLine line = 
  case (strSplitAll "\t" line) of
    [t1, t2, t3, t4] -> Just $ parseToks t1 t2 t3 t4
    otherwise -> Nothing
  where
    parseToks front path host port =
      ( (strHead front)   -- Type
      , (strDrop 1 front) -- User Text
      , (strTrim path)
      , (strTrim host)
      , (strTrim port) )

{---------------}
{------ IO -----}
{---------------}

gopherGetRaw :: GopherUrl -> IO ByteString
gopherGetRaw (host, path, port) =
  getAddrInfo (Just addrInfoHints) (Just host) (Just port)
  >>= return . addrAddress . head 
  >>= \addr -> socket AF_INET Stream 0 
    >>= \sock -> connect sock addr
      >> sendAll sock (appendCRLF $ C.pack path)
      >> recvAll sock

urlToFilePath :: Bool ->  GopherUrl -> FilePath
urlToFilePath isMenu (host, path, port) = 
  joinPath $ 
    [host] ++
    (sanitizePath path) ++
    (if isMenu then ["gophermap"] else [])

save :: ByteString -> GopherUrl -> Bool -> IO ()
save bs url isMenu =
  let out = urlToFilePath isMenu url in
  createDirectoryIfMissing True (dropFileName out) >>
  withFile out WriteMode writeIt
  where
    writeIt handle = hPut handle bs >> hFlush handle

getAndSaveMenu :: GopherUrl -> IO [MenuLine]
getAndSaveMenu url = 
  gopherGetRaw url 
    >>= \bs -> save bs url True
    >> return (parseMenu bs)

getAndSaveFile :: GopherUrl -> IO ByteString
getAndSaveFile url = 
  gopherGetRaw url 
    >>= \bs -> save bs url False 
    >> return bs

-- If file exists on disk, read it instead of accessing network
getAndSaveMenuCheckExists :: GopherUrl -> IO [MenuLine]
getAndSaveMenuCheckExists url = 
  let path = urlToFilePath True url in
  doesPathExist path
  >>= \exists -> 
    if exists 
      then (readFile path >>= return . parseMenu . C.pack)
      else getAndSaveMenu url

getRecursively :: GopherUrl -> Config -> IO [Remote]
getRecursively url conf =
  crawlMenu url conf (maxDepth conf) []

crawlMenu :: GopherUrl -> Config -> Int -> [GopherUrl] -> IO [Remote]
crawlMenu url conf depth history =
  putStrLn ((nchrs '-' ((maxDepth conf) - depth)) ++ "(menu) " ++ (urlToString url))
  >> getMenuMaybeFromDisk url
  >>= debugLog
  >>= return . map remotifyLine . filter okLine
  >>= mapM (getRemotes conf depth history)
  >>= return . concat
  where
    getMenuMaybeFromDisk = 
      if (clobber conf) 
        then getAndSaveMenu
        else getAndSaveMenuCheckExists
    okLine ml = 
      notInHistory ml && okHost ml
    notInHistory ml = 
      (mlToUrl ml) `notElem` history
    okHost ml =
      if (spanHosts conf) then True 
                          else sameHost url (mlToUrl ml)

getRemotes :: Config -> Int -> [GopherUrl] -> Remote -> IO [Remote]
getRemotes conf depth history remote = 
  case remote of
    RemoteFile url -> return [remote]
    RemoteMenu url -> fmap ((:) remote) nextRemotes
      where 
        nextRemotes = if atMaxDepth then return [] else getNextRemotes
        atMaxDepth = (depth - 1) == 0
        getNextRemotes = crawlMenu url conf (depth - 1) (url : history)

remotifyLine :: MenuLine -> Remote
remotifyLine ml = 
  if (lineIsMenu ml) 
    then RemoteMenu (mlToUrl ml)
    else RemoteFile (mlToUrl ml)
    
gopherGetMenu :: GopherUrl -> IO (ByteString, [MenuLine])
gopherGetMenu url = 
  debugLog ("gopherGetMenu: " ++ (urlToString url))
  >> gopherGetRaw url 
  >>= \bytes -> return $ (bytes, parseMenu bytes)

getAndSaveFilePrintStatus :: GopherUrl -> IO ()
getAndSaveFilePrintStatus url =
  putStrLn ("(file) " ++ (urlToString url)) >>
  getAndSaveFile url >>
  hFlush stdout

{-----------------}
{------ Main -----}
{-----------------}

-- Get Argv, turn it into a Config
main :: IO ()
main = Env.getArgs 
  >>= debugLog . argvToConfig
  >>= main'

-- Check sanity of config and args
main' :: ([String], Config) -> IO ()
main' (args, conf)
  | helpFlag || noArg = showUsage 
  | failedParsingUrl = putStrLn "Cannot Parse URL(s)" >> showUsage
  | otherwise = mapM_ (main'' conf) parsedUrls >> putStrLn ":: Done"
  where 
    helpFlag = (help conf)
    noArg = (length args) == 0
    parsedUrls = catMaybes $ map parseGopherUrl args
    failedParsingUrl = (length args) /= (length parsedUrls)

printFiles files = 
  mapM (putStrLn . show) files >>
  return files

-- Handle each gopher URL
main'' :: Config -> GopherUrl -> IO ()
main'' conf url
  | (recursive conf) = 
    putStrLn ":: Downloading menus" >>
    getRecursively url conf
    >>= return . filter isRemoteFile
    >>= \allRemotes -> filterM (goodRemote conf) allRemotes
      >>= \remotes -> return (map remoteToUrl remotes)
        >>= downloadSaveUrls ((length allRemotes) - (length remotes))
  | otherwise =
    putStrLn ":: Downloading single file" >>
    mapM_ getAndSaveFilePrintStatus [url]

-- TODO: Assumes it's a normal, make correct???
urlExistsLocally :: GopherUrl -> IO Bool
urlExistsLocally url = 
  doesPathExist (urlToFilePath False url)
  >>= debugLog

-- True = Remove it
goodRemote :: Config -> Remote -> IO Bool
goodRemote conf remote 
  | (onlyMenus conf) = return False
  | (clobber conf) = return True
  | otherwise = fmap not (urlExistsLocally (remoteToUrl remote))

downloadSaveUrls :: Int -> [GopherUrl]-> IO ()
downloadSaveUrls skipping fileUrls = 
  let nFiles = (length fileUrls)
      skippingMsg = "(skipping " ++ (show skipping) ++ " on disk)"
      msg = (":: Downloading " ++ (show nFiles) ++ " files ")
  in
  putStrLn (msg ++ skippingMsg) >>
  mapM_ getAndSaveFilePrintStatus fileUrls
