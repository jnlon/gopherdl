import qualified Data.Set as Set
import qualified Text.Regex.PCRE as Regex
import qualified Text.Regex.PCRE.String as PCRE
import Data.List
import Data.Maybe
import Data.Strings
import Network.URI (URI, parseURI, uriAuthority, uriPath, uriRegName, uriPort)
import Control.Exception
import Control.Monad
import System.IO
import System.Directory (createDirectoryIfMissing, doesPathExist)
import Data.ByteString (hPut)
import System.FilePath.Posix
import qualified System.Environment as Env
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Char8 (ByteString)
import System.Console.GetOpt (OptDescr(Option), ArgDescr(NoArg, ReqArg),
                              getOpt, ArgOrder(RequireOrder), usageInfo)
import Network.Socket (Socket, AddrInfo, addrSocketType, addrAddress, 
                       close, connect, defaultHints, SocketType(Stream), 
                       getAddrInfo, Family(AF_INET), socket)

{- TODO
  - Implement -w
  - compile time debug switch from build tool (stack?) -}

-- type text \t path \t host \t port \r\n
type MenuLine = (Char, ByteString, ByteString, ByteString, ByteString)

data UrlType = 
    File 
  | Menu deriving (Show, Eq, Ord)

-- host path port
data GopherUrl = GopherUrl 
  { host :: String
  , path :: String
  , port :: String
  , urlT :: UrlType } deriving (Show, Eq, Ord)

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
    | AcceptRegex String
    | Delay Float deriving (Eq, Show, Ord)

data Config = Config
 {  recursive :: Bool
  , maxDepth :: Int
  , spanHosts :: Bool
  , help :: Bool
  , clobber :: Bool
  , onlyMenus :: Bool
  , constrainPath :: Bool
  , rejectRegex :: String
  , acceptRegex :: String
  , delay :: Float } deriving Show

okByAcceptRegex :: Config -> GopherUrl -> IO Bool
okByAcceptRegex conf url = do
  compAcceptRegex <- compileRegex (acceptRegex conf)
  case compAcceptRegex of
    Just re -> urlMatchesRegex re url
    otherwise -> return True

okByRejectRegex :: Config -> GopherUrl -> IO Bool
okByRejectRegex conf url = do
  compRejectRegex <- compileRegex (rejectRegex conf)
  case compRejectRegex of
    Just re -> fmap not $ urlMatchesRegex re url
    otherwise -> return True

okByFileExists :: Config -> GopherUrl -> IO Bool
okByFileExists conf url
  | (clobber conf)       = return True 
  | (not (clobber conf)) = fmap not $ doesPathExist (urlToFilePath url)

okByHost :: Config -> GopherUrl -> GopherUrl -> Bool
okByHost conf url1 url2
  | (spanHosts conf)       = True
  | (not (spanHosts conf)) = sameHost url1 url2

okByPath :: Config -> GopherUrl -> GopherUrl -> Bool
okByPath conf url1 url2
  | (constrainPath conf)       = commonPathBase url1 url2
  | (not (constrainPath conf)) = True

okByType :: Config -> GopherUrl -> Bool
okByType conf url
  | (onlyMenus conf)       = (urlT url) == Menu
  | (not (onlyMenus conf)) = True

{---------------------}
{------ Helpers ------}
{---------------------}

_debug = False

(&&>) :: Monad m => m Bool -> m Bool -> m Bool
(&&>) = liftM2 (&&)

debugLog :: Show a => String -> a -> IO a
debugLog what a =
  (if _debug
    then (hPutStrLn stderr (what ++ " => " ++ (show a)) 
         >> hFlush stderr)
    else return ()) >> return a

recvAllToFile :: Socket -> FilePath -> IO ()
recvAllToFile sock path =
  withFile path WriteMode (recvAllToHandle sock)

recvAllToHandle :: Socket -> Handle -> IO ()
recvAllToHandle sock hndl =
  recv sock 4096 
  >>= \bytes -> 
    hPut hndl bytes -- Write bytes to file
    >> recvMore bytes
  where 
   recvMore bytes =
     if (C.length bytes) == 0
       then close sock
       else recvAllToHandle sock hndl

recvAll :: Socket -> IO ByteString
recvAll sock = 
  recv sock 4096
  >>= recvMore 
  >>= \bytes -> close sock >> return bytes
  where 
   recvMore bytes =
     if (C.length bytes) == 0
       then return bytes
       else recvAll sock >>= return . C.append bytes

appendCRLF :: String -> String
appendCRLF s = s ++ "\r\n"

addrInfoHints :: AddrInfo
addrInfoHints = defaultHints { addrSocketType = Stream }

isFileUrl url = 
  (urlT url) == File

showUsage = 
  putStr $ usageInfo "gopherdl [options] [urls]" optionSpec

sameHost url1 url2 = 
  (host url1) == (host url2)

fixProto urlStr = 
  let hasNoProto urlStr = (snd (strSplit "://" urlStr)) == "" in
  if (hasNoProto urlStr) then ("gopher://" ++ urlStr) else urlStr

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

compileRegex :: String -> IO (Maybe Regex.Regex)
compileRegex "" = return Nothing
compileRegex reStr = 
  PCRE.compile PCRE.compBlank PCRE.execBlank reStr
  >>= extractRegex
  where 
    extractRegex (Left (offset, errs)) = putStrLn errs >> hFlush stdout >> (return Nothing)
    extractRegex (Right regex) = return (Just regex)

optionSpec = 
  let argMaxDepth depth = (MaxDepth (read depth::Int)) 
      argDelay delay = (Delay (read delay::Float)) 
      rejectRegex s = (RejectRegex s)
      acceptRegex s = (AcceptRegex s)
  in
  [ Option "r" [] (NoArg Recursive) "Enable recursive downloads"
  , Option "l" [] (ReqArg argMaxDepth "n") "Maximum depth in recursive downloads"
  , Option "s" [] (NoArg SpanHosts) "Span hosts on recursive downloads"
  , Option "h" [] (NoArg Help) "Show this help"
  , Option "c" [] (NoArg Clobber) "Enable file clobbering (overwrite existing)"
  , Option "m" [] (NoArg OnlyMenus) "Only download gopher menus"
  , Option "p" [] (NoArg ConstrainPath) "Only descend into child directories"
  , Option "w" [] (ReqArg argDelay "secs") "Delay between downloads"
  , Option "R" [] (ReqArg rejectRegex "pcre") "Reject URL based on pcre" 
  , Option "A" [] (ReqArg acceptRegex "pcre") "Accept URL based on pcre" ]

isMaxDepth (MaxDepth _) = True
isMaxDepth otherwise = False

isDelay (Delay _) = True
isDelay otherwise = False

isRejectRegex (RejectRegex _) = True
isRejectRegex otherwise = False

isAcceptRegex (AcceptRegex _) = True
isAcceptRegex otherwise = False

findMaxDepth def options =
  case (find isMaxDepth options) of
    Just (MaxDepth d) -> d
    _ -> def

findDelay def options =
  case (find isDelay options) of
    Just (Delay d) -> d
    _ -> def

findRejectRegex :: String -> [Flag] -> String
findRejectRegex def options =
  case (find isRejectRegex options) of
    Just (RejectRegex restr) -> restr
    _ -> def

findAcceptRegex :: String -> [Flag] -> String
findAcceptRegex def options =
  case (find isAcceptRegex options) of
    Just (AcceptRegex restr) -> restr
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
           , rejectRegex = findRejectRegex "" options
           , acceptRegex = findAcceptRegex "" options })
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
        { host = uriRegName auth
        , path = uriPath uri ?>> "/"
        , port = strDrop 1 (uriPort auth) ?>> "70"
        , urlT = Menu }
    otherwise -> Nothing
  where 
    (?>>) a def = if a == "" then def else a

readFileU8 path = 
  openFile path ReadMode
  >>= \h -> hSetEncoding h char8
  >> hGetContents h

{------------------------}
{----- Menu Parsing -----}
{------------------------}

mlToUrl :: MenuLine -> GopherUrl
mlToUrl (t, _, _path, _host, _port) =
  GopherUrl { host = C.unpack _host 
            , path = C.unpack _path
            , port = C.unpack _port
            , urlT = (if t == '1' then Menu else File)}

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
      >> sendAll sock (C.pack $ appendCRLF (path url))
      >> recvAll sock

urlToFilePath :: GopherUrl -> FilePath
urlToFilePath url = 
  joinPath $
    ([(host url) ++ ":" ++ (port url)] ++
    (sanitizePath (path url)) ++
    [(if ((urlT url) == Menu) then "gophermap" else "")])

save :: ByteString -> GopherUrl -> IO ()
save bs url =
  let out = urlToFilePath url in
  createDirectoryIfMissing True (dropFileName out) >>
  withFile out WriteMode writeIt
  where
    writeIt handle = hPut handle bs >> hFlush handle

getAndSaveMenuNet :: GopherUrl -> IO [MenuLine]
getAndSaveMenuNet url = 
  gopherGetRaw url 
    >>= \bs -> save bs url
    >> return (parseMenu bs)

getAndSaveFile :: GopherUrl -> IO ByteString
getAndSaveFile url = 
  gopherGetRaw url 
    >>= \bs -> save bs url
    >> return bs

-- If file exists on disk, read it instead of accessing network
getAndSaveMenuMaybeFromDisk :: GopherUrl -> IO [MenuLine]
getAndSaveMenuMaybeFromDisk url = 
  let path = urlToFilePath url in
  doesPathExist path >>= \exists -> 
    if exists 
      then getAndSaveMenuDisk path
      else getAndSaveMenuNet url
  where
    getAndSaveMenuDisk path = 
      readFileU8 path >>= return . parseMenu . C.pack

getMenuFromDiskOrNet conf
  | (clobber conf)       = getAndSaveMenuNet
  | (not (clobber conf)) = getAndSaveMenuMaybeFromDisk

getRecursively :: GopherUrl -> Config -> IO [GopherUrl]
getRecursively url conf =
  crawlMenu url conf (maxDepth conf) Set.empty

-- Apply filters only relevent to a URL
goodMenuUrl conf currentMenuUrl url = 
  okByRejectRegex conf url >>= \okRegex -> 
    return $ 
      okRegex
      && okByPath conf currentMenuUrl url 
      && okByHost conf currentMenuUrl url
      && okByType conf url

goodFileUrl conf url =
  okByAcceptRegex conf url >>= \okAcceptRegex -> 
  okByRejectRegex conf url >>= \okRejectRegex -> 
  okByFileExists conf url >>= \okFileExists ->
  return $ 
    okAcceptRegex 
    && okRejectRegex
    && okFileExists
    && okByType conf url 

-- crawlFilter conf refUrl url =

putCrawlStatusLn conf depth refUrl = 
  let depthLeft = ((maxDepth conf) - depth) 
      depthStr = "[" ++ (show depthLeft) ++ "/" ++ (show (maxDepth conf)) ++ "] " 
  in
  putStrLn $ "(menu) " ++ depthStr ++ (urlToString refUrl)


crawlMenu :: GopherUrl -> Config -> Int -> Set.Set GopherUrl -> IO [GopherUrl]
crawlMenu refUrl conf depth history =
  let notInHistory mlUrl = mlUrl `Set.notMember` history
  in
  putCrawlStatusLn conf depth refUrl >>
  getMenuFromDiskOrNet conf refUrl
  >>= return . filter notInHistory . map mlToUrl   -- Convert lines to urls 
  >>= filterM (goodMenuUrl conf refUrl)            -- Filter urls by config 
  >>= debugLog "GOODURLS\n"
  >>= \remotes ->
    let newHistory = Set.union (Set.fromList remotes) history  in
    mapM (getRemotes conf depth newHistory) remotes
    >>= return . concat

getRemotes :: Config -> Int -> Set.Set GopherUrl -> GopherUrl -> IO [GopherUrl]
getRemotes conf depth history url = 
  debugLog "getRemotes" url >>
  case (urlT url) of
    File -> return [url]
    Menu -> fmap ((:) url) nextRemotes
      where 
        atMaxDepth = (depth - 1) == 0
        nextRemotes = if atMaxDepth then return [] else getNextRemotes
        getNextRemotes = crawlMenu url conf (depth - 1) history

gopherGetMenu :: GopherUrl -> IO (ByteString, [MenuLine])
gopherGetMenu url = 
  debugLog "gopherGetMenu" ("gopherGetMenu: " ++ (urlToString url))
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
    putStrLn ":: Downloading menus" >>
    getRecursively url conf
    >>= return . filter isFileUrl
    >>= filterM (goodFileUrl conf)
    >>= getAndSaveUrls
  | otherwise =
    putStrLn ":: Downloading single file" >>
    mapM_ getAndSaveFilePrintStatus [url]

urlMatchesRegex :: PCRE.Regex -> GopherUrl -> IO Bool
urlMatchesRegex re url = do
  result <- PCRE.execute re (urlToString url)
  return $ 
    case result of
      Right (Just _) -> True
      otherwise -> False

getAndSaveUrls :: [GopherUrl]-> IO ()
getAndSaveUrls fileUrls = 
  putStrLn (":: Downloading " ++ (show (length fileUrls)) ++ " files ") >>
  mapM_ getAndSaveFilePrintStatus fileUrls
