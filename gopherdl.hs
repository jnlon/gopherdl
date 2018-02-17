import qualified Data.Set as Set
import qualified Text.Regex.PCRE as Regex
import qualified Text.Regex.PCRE.String as PCRE
import Data.List
import Data.Maybe
import Data.Strings
import Network.URI (URI, parseURI, uriAuthority, uriPath, uriRegName, uriPort)
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

{- TODO
  - Implement -w
  - Find a smarter way of applying common filters to gophermaps and files
  - rename and move myReadFile to helpers
  - compile time debug switch from build tool (stack?)
  - Smarter way of passing in regex... Maybe a "recursive options" data struct?
  - Can regex be an empty string? That would declutter maybes and is probably fast...
    - Clean up regex handling/conversion in general!
-}

sendAll = BsNet.sendAll
type ByteString = C.ByteString

-- type text \t path \t host \t port \r\n
type MenuLine = (Char, ByteString, ByteString, ByteString, ByteString)
-- host path port
data GopherUrl = GopherUrl 
  { host :: String
  , path :: String
  , port :: String } deriving (Show, Eq, Ord)

data Flag = 
      Recursive
    | MaxDepth Int
    | SpanHosts
    | Help
    | Clobber
    | OnlyMenus
    | NoMenus
    | ConstrainPath
    | RejectRegex String
    | Delay Float deriving (Eq, Show)

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
  , constrainPath :: Bool
  , rejectRegex :: Maybe String
  , delay :: Float } deriving Show

{---------------------}
{------ Helpers ------}
{---------------------}

_debug = False

debugLog :: Show a => a -> IO a
debugLog a =
  (if _debug
    then (hPutStrLn stderr ("DEBUG: " ++ show a) 
         >> hFlush stderr)
    else return ()) >> return a

recvAll :: Socket -> IO ByteString
recvAll sock = 
  BsNet.recv sock 4096
  >>= recvMore 
  >>= \bytes -> close sock >> return bytes
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
  (host url1) == (host url2)

fixProto s = 
  let noProto s = (snd (strSplit "://" s)) == "" in
  if (noProto s) then ("gopher://" ++ s) else s

commonPathBase prevUrl nextUrl = 
  strStartsWith (path nextUrl) (path prevUrl) 

implies cond fn =
  if cond then fn else True

sanitizePath path = 
  let nonEmptyString = (/=) 0 . sLen in
  filter nonEmptyString $ strSplitAll "/" path

parseGopherUrl :: String -> Maybe GopherUrl
parseGopherUrl =
  uriToGopherUrl . parseURI . fixProto

{--------------------------}
{------ Argv Parsing ------}
{--------------------------}

compileRegex :: Maybe String -> IO (Maybe Regex.Regex)
compileRegex str = 
  case str of
    Just str ->
      ((PCRE.compile PCRE.compBlank PCRE.execBlank str)
      >>= \cr -> 
        (case cr of
          Left (offset, string) -> putStrLn string >> hFlush stdout >> (return Nothing)
          Right regex -> (return (Just regex))))
    _ -> return Nothing

optionSpec = 
  let argMaxDepth depth = (MaxDepth (read depth::Int)) 
      argDelay delay = (Delay (read delay::Float)) 
      rejectRegex s = (RejectRegex s)
  in
  [ Option "r" [] (NoArg Recursive) "Enable recursive downloads"
  , Option "l" [] (ReqArg argMaxDepth "n") "Maximum depth in recursive downloads"
  , Option "s" [] (NoArg SpanHosts) "Span hosts on recursive downloads"
  , Option "h" [] (NoArg Help) "Show this help"
  , Option "c" [] (NoArg Clobber) "Enable file clobbering (overwrite existing)"
  , Option "m" [] (NoArg OnlyMenus) "Only download gopher menus"
  , Option "p" [] (NoArg ConstrainPath) "Only descend into child directories"
  , Option "w" [] (ReqArg argDelay "secs") "Delay between downloads"
  , Option "R" [] (ReqArg rejectRegex "pcre") "Reject URL based on pcre" ]

isMaxDepth (MaxDepth _) = True
isMaxDepth otherwise = False

isDelay (Delay _) = True
isDelay otherwise = False

isRejectRegex (RejectRegex _) = True
isRejectRegex otherwise = False

findMaxDepth def options =
  case (find isMaxDepth options) of
    Just (MaxDepth d) -> d
    _ -> def

findDelay def options =
  case (find isDelay options) of
    Just (Delay d) -> d
    _ -> def

findRejectRegex :: Maybe String -> [Flag] -> Maybe String
findRejectRegex def options =
  case (find isRejectRegex options) of
    Just (RejectRegex restr) -> Just restr
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
           , constrainPath = has ConstrainPath
           , delay = findDelay 0.0 options
           , rejectRegex = findRejectRegex Nothing options})
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
    Just auth -> 
      Just $ GopherUrl
        { host = (getHost auth)
        , path = (getPath uri)
        , port = (getPort auth) }
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
mlToUrl (_, _, _path, _host, _port) =
  GopherUrl { host = C.unpack _host 
            , path = C.unpack _path
            , port = C.unpack _port }

urlToString :: GopherUrl -> String
urlToString url =
  (host url) ++ ":" ++ (port url) ++ (path url)

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
gopherGetRaw url =
  getAddrInfo (Just addrInfoHints) (Just (host url)) (Just (port url))
  >>= return . addrAddress . head 
  >>= \addr -> socket AF_INET Stream 0 
    >>= \sock -> connect sock addr
      >> sendAll sock (appendCRLF $ C.pack (path url))
      >> recvAll sock

urlToFilePath :: Bool ->  GopherUrl -> FilePath
urlToFilePath isMenu url = 
  joinPath $
    ([(host url) ++ ":" ++ (port url)] ++
    (sanitizePath (path url)) ++
    [(if isMenu then "gophermap" else "")])

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
      then (myReadFile path >>= return . parseMenu . C.pack)
      else getAndSaveMenu url
  where 
    myReadFile path =
      openFile path ReadMode
      >>= \h -> hSetEncoding h char8
      >> hGetContents h

getRecursively :: GopherUrl -> Config -> IO (Maybe Regex.Regex) -> IO [Remote]
getRecursively url conf iocre =
  iocre >>= \cr ->
    crawlMenu url conf (maxDepth conf) Set.empty cr

crawlMenu :: GopherUrl -> Config -> Int -> Set.Set GopherUrl -> Maybe (Regex.Regex) -> IO [Remote]
crawlMenu url conf depth history cr =
  let depthLeft = ((maxDepth conf) - depth) 
      depthMsg = "[" ++ (show depthLeft) ++ "/" ++ (show (maxDepth conf)) ++ "] " 
  in
  putStrLn ("(menu) " ++ depthMsg ++ (urlToString url))
  >> getMenuMaybeFromDisk url
  >>= debugLog
  >>= filterM okUrl
  >>= return . map remotifyLine . filter okLine
  >>= \remotes ->
    let urlSet = (Set.fromList (map remoteToUrl remotes)) in
    mapM (getRemotes conf depth (Set.union urlSet history) cr) remotes
    >>= return . concat
  where
    getMenuMaybeFromDisk = 
      if (clobber conf) 
        then getAndSaveMenu
        else getAndSaveMenuCheckExists
    okLine ml = 
      notInHistory ml && okHost ml && okPath ml
    okUrl ml = 
      if (isJust cr) 
        then fmap not (urlMatchesRegex (fromJust cr) (mlToUrl ml))
        else return True
    notInHistory ml = 
      (mlToUrl ml) `Set.notMember` history
    okPath ml =
      (constrainPath conf) `implies` (commonPathBase url (mlToUrl ml))
    okHost ml =
      not (spanHosts conf) `implies` (sameHost url (mlToUrl ml))

getRemotes :: Config -> Int -> Set.Set GopherUrl -> Maybe (Regex.Regex) -> Remote -> IO [Remote]
getRemotes conf depth history cr remote = 
  case remote of
    RemoteFile url -> return [remote]
    RemoteMenu url -> fmap ((:) remote) nextRemotes
      where 
        nextRemotes = if atMaxDepth then return [] else getNextRemotes
        atMaxDepth = (depth - 1) == 0
        getNextRemotes = crawlMenu url conf (depth - 1) history cr

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
  putStr ("(file) " ++ (urlToString url) ++ " ") >>
  hFlush stdout >>
  getAndSaveFile url 
  >>= \bs -> putStrLn ("(" ++ (show ((C.length bs) `div` 1000)) ++ "k)") >>
  hFlush stdout

{-----------------}
{------ Main -----}
{-----------------}

-- Get Argv, turn it into a Config
main :: IO ()
main = Env.getArgs 
  >>= main' . argvToConfig

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

-- Handle each gopher URL
main'' :: Config -> GopherUrl -> IO ()
main'' conf url
  | (recursive conf) = 
    let cre = (compileRegex (rejectRegex conf)) in
    cre >>
    putStrLn ":: Downloading menus" >>
    getRecursively url conf cre
    >>= return . filter isRemoteFile
    >>= \allRemotes -> filterM (goodRemote conf cre) allRemotes
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

urlMatchesRegex :: PCRE.Regex -> GopherUrl -> IO Bool
urlMatchesRegex cr url =
  stringMatchesRegex cr (urlToString url)

stringMatchesRegex :: PCRE.Regex -> String -> IO Bool
stringMatchesRegex re url =
  PCRE.execute re url
  >>= \result ->
    return $ 
      (case result of
        Left err -> False
        Right (Just _) -> True
        Right _ -> False)

goodRemote :: Config -> IO (Maybe Regex.Regex) -> Remote -> IO Bool
goodRemote conf iore remote = 
  iore >>= \re ->
    sequence [(fmap not (regexMatches re remote)), (goodRemote' conf remote)]
    >>= return . all (==True)

-- True = Remove it
goodRemote' :: Config -> Remote -> IO Bool
goodRemote' conf remote 
  | (onlyMenus conf) = return False
  | (clobber conf) = return True
  | otherwise = fmap not (urlExistsLocally (remoteToUrl remote))

regexMatches re remote
  | isJust re = urlMatchesRegex (fromJust re) (remoteToUrl remote)
  | otherwise = return False

downloadSaveUrls :: Int -> [GopherUrl]-> IO ()
downloadSaveUrls skipping fileUrls = 
  let nFiles = (length fileUrls)
      skippingMsg = "(skipping " ++ (show skipping) ++ ")"
      msg = (":: Downloading " ++ (show nFiles) ++ " files ")
  in
  putStrLn (msg ++ skippingMsg) >>
  mapM_ getAndSaveFilePrintStatus fileUrls
