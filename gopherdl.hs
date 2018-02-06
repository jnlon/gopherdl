import Data.List
import Data.Maybe
import Data.Strings
import Network.URI
import Network.Socket
import Control.Exception
import Control.Monad
import System.IO
import System.Directory (createDirectoryIfMissing)
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

_debug = False

debugLog :: Show a => a -> IO a
debugLog a =
  (if _debug
    then putStrLn $ ("DEBUG: " ++ show a)
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

fixProto s = 
  let noProto s = (snd (strSplit "://" s)) == "" in
  if (noProto s) then ("gopher://" ++ s) else s

sanitizePath path = 
  let nonEmptyString = (/=) 0 . sLen in
  filter nonEmptyString $ strSplitAll "/" path

hostOfUrl :: GopherUrl -> String
hostOfUrl (host, _, _) = host

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
    _ -> Nothing
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

urlToFilePath :: GopherUrl -> Bool -> FilePath
urlToFilePath (host, path, port) isMenu = 
  joinPath $ 
    [host] ++
    (sanitizePath path) ++
    (if isMenu then ["gophermap"] else [])

save :: ByteString -> GopherUrl -> Bool -> IO ()
save bs url isMenu =
  let out = urlToFilePath url isMenu in
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

getRecursively :: GopherUrl -> Config -> IO [Remote]
getRecursively url conf =
  crawlMenu url conf (maxDepth conf) []

sameHost url1 url2 =
  (hostOfUrl url1) /= (hostOfUrl url1)

crawlMenu :: GopherUrl -> Config -> Int -> [GopherUrl] -> IO [Remote]
crawlMenu url conf depth history =
  putStrLn ("(menu) " ++ (urlToString url))
  >> getAndSaveMenu url
  >>= debugLog
  >>= return . map remotifyLine . filter okLine
  >>= mapM (getRemotes conf depth history)
  >>= return . concat
  where
    okLine ml = 
      notInHistory ml && okHost ml
    notInHistory ml = 
      (mlToUrl ml) `notElem` history
    okHost ml = 
      not (spanHosts conf) && sameHost url (mlToUrl ml)

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
  debugLog ("gopherGetMenu: " ++ (urlToString url)) >>
  gopherGetRaw url 
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
main =
  Env.getArgs 
  >>= main' . argvToConfig

-- Check sanity of config and args
main' :: ([String], Config) -> IO ()
main' (args, conf)
  | (help conf) || (length args) == 0 = showUsage 
  | (length args) /= (length urls) = putStrLn "Cannot Parse URL(s)" >> showUsage
  | otherwise = mapM_ (main'' conf) urls
    where 
      urls = catMaybes $ map (uriToGopherUrl . parseURI . fixProto) args

-- Handle each gopher URL
main'' :: Config -> GopherUrl -> IO ()
main'' conf url =
  if (recursive conf) then
    putStrLn ":: Building Menu Tree" >>
    getRecursively url conf
    >>= return . map remoteToUrl . filter isRemoteFile
    >>= \fileUrls ->
      putStrLn (":: Downloading Files: " ++ (show (length fileUrls)))
      >> mapM_ getAndSaveFilePrintStatus fileUrls
  else
    putStrLn ":: Downloading Single File" >>
    mapM_ getAndSaveFilePrintStatus [url]
    -- >>= debugLog >> return ()

