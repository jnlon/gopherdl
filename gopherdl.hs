import qualified Data.Set as Set
import qualified Text.Regex.PCRE as Regex
import qualified Text.Regex.PCRE.String as PCRE
import Data.List
import Data.Maybe
import Data.Strings
import Network.URI (URI, URIAuth, parseURI, uriAuthority,
                    uriPath, uriRegName, uriPort)
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
    | AcceptRegex String deriving (Eq, Show, Ord)

data Config = Config
 {  recursive :: Bool
  , maxDepth :: Int
  , spanHosts :: Bool
  , help :: Bool
  , clobber :: Bool
  , onlyMenus :: Bool
  , constrainPath :: Bool
  , rejectRegex :: String
  , acceptRegex :: String } deriving Show

{---------------------}
{------ Helpers ------}
{---------------------}

_debug = False

debugLog :: Show a => String -> a -> IO a
debugLog what a =
  (if _debug
    then (hPutStrLn stderr (what ++ " => " ++ (show a)) 
         >> hFlush stderr)
    else return ()) >> return a

isFileUrl url = (urlT url) == File
appendCRLF s = s ++ "\r\n"
putStrLnFl s = putStrLn s >> hFlush stdout
putStrFl s = putStr s >> hFlush stdout
addrInfoHints = defaultHints { addrSocketType = Stream }
showUsage = putStr $ usageInfo "gopherdl [options] [urls]" optionSpec
notInSet set e = Set.notMember e set
sameHost url1 url2 = (host url1) == (host url2)
(?>>) str _default = if str == "" then _default else str

commonPathBase prevUrl nextUrl = 
  strStartsWith (path nextUrl) (path prevUrl) 

sanitizePath path = 
  let nonEmptyString = (/=) 0 . sLen in
  filter nonEmptyString $ strSplitAll "/" path

fixProto urlStr = 
  let hasNoProto urlStr = (snd (strSplit "://" urlStr)) == "" in
  if (hasNoProto urlStr) then ("gopher://" ++ urlStr) else urlStr

uriToGopherUrl :: Maybe URI -> Maybe GopherUrl
uriToGopherUrl Nothing = Nothing
uriToGopherUrl (Just uri) = uriToGopherUrl' uri (uriAuthority uri)

uriToGopherUrl' :: URI -> Maybe URIAuth -> Maybe GopherUrl
uriToGopherUrl' uri Nothing = Nothing
uriToGopherUrl' uri (Just auth) = 
  Just $ GopherUrl
    { host = uriRegName auth
    , path = uriPath uri ?>> "/"
    , port = strDrop 1 (uriPort auth) ?>> "70"
    , urlT = Menu }
    
parseGopherUrl :: String -> Maybe GopherUrl
parseGopherUrl =
  uriToGopherUrl . parseURI . fixProto

urlToFilePath :: GopherUrl -> FilePath
urlToFilePath url = 
  joinPath $
    ([(host url) ++ ":" ++ (port url)] ++
    (sanitizePath (path url)) ++
    [(if ((urlT url) == Menu) then "gophermap" else "")])

compileRegex :: String -> IO (Maybe Regex.Regex)
compileRegex "" = return Nothing
compileRegex reStr = 
  PCRE.compile PCRE.compBlank PCRE.execBlank reStr
  >>= extractRegex
  where 
    extractRegex (Left (offset, errs)) = putStrLnFl errs >> (return Nothing)
    extractRegex (Right regex) = return (Just regex)

{--------------------------}
{------ URL Filtering -----}
{--------------------------}

urlMatchesRegex :: PCRE.Regex -> GopherUrl -> IO Bool
urlMatchesRegex re url = do
  result <- PCRE.execute re (urlToString url)
  return $ 
    case result of
      Right (Just _) -> True
      otherwise -> False

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

-- Filters to apply during menu crawl
goodMenuUrl conf currentMenuUrl url = 
  okByRejectRegex conf url >>= \okRegex -> 
    return $ all (==True)
      [ okRegex
      , okByPath conf currentMenuUrl url 
      , okByHost conf currentMenuUrl url
      , okByType conf url ]

-- Filters for bulk file download after menu crawl
goodFileUrl conf url =
  okByAcceptRegex conf url >>= \okAcceptRegex -> 
  okByRejectRegex conf url >>= \okRejectRegex -> 
  okByFileExists conf url >>= \okFileExists ->
  return $ all (==True)
    [ okAcceptRegex 
    , okRejectRegex
    , okFileExists
    , okByType conf url ]

{--------------------------}
{------ Argv Parsing ------}
{--------------------------}

optionSpec = 
  let argMaxDepth depth = (MaxDepth (read depth::Int)) 
  in
  [ Option "r" [] (NoArg Recursive) "Enable recursive downloads"
  , Option "l" [] (ReqArg argMaxDepth "n") "Maximum depth in recursive downloads"
  , Option "s" [] (NoArg SpanHosts) "Span hosts on recursive downloads"
  , Option "h" [] (NoArg Help) "Show this help"
  , Option "c" [] (NoArg Clobber) "Enable file clobbering (overwrite existing)"
  , Option "m" [] (NoArg OnlyMenus) "Only download gopher menus"
  , Option "p" [] (NoArg ConstrainPath) "Only descend into child directories"
  , Option "R" [] (ReqArg RejectRegex "pcre") "Reject URL based on pcre" 
  , Option "A" [] (ReqArg AcceptRegex "pcre") "Accept URL based on pcre" ]

isMaxDepth (MaxDepth _) = True
isMaxDepth otherwise = False

isRejectRegex (RejectRegex _) = True
isRejectRegex otherwise = False

isAcceptRegex (AcceptRegex _) = True
isAcceptRegex otherwise = False

configFromGetOpt :: ([Flag], [String], [String]) -> ([String], Config)
configFromGetOpt (options, arguments, errors) = 
  ( arguments, 
    Config { recursive = has Recursive
           , maxDepth = maybeGetOpt isMaxDepth (getIntOr 99)
           , spanHosts = has SpanHosts
           , help = has Help
           , clobber = has Clobber
           , onlyMenus = has OnlyMenus
           , constrainPath = has ConstrainPath
           , rejectRegex = maybeGetOpt isRejectRegex (getStringOr "") 
           , acceptRegex = maybeGetOpt isAcceptRegex (getStringOr "") })
  where 
    has opt = opt `elem` options 
    maybeGetOpt finder getter = getter (find finder options)

    getIntOr _default (Just (MaxDepth d)) = d
    getIntOr _default _ = _default

    getStringOr _default (Just (AcceptRegex s)) = s
    getStringOr _default (Just (RejectRegex s)) = s
    getStringOr _default _ = _default

parseWithGetOpt :: [String] -> ([Flag], [String], [String])
parseWithGetOpt argv = getOpt RequireOrder optionSpec argv

argvToConfig :: [String] -> ([String], Config)
argvToConfig = configFromGetOpt . parseWithGetOpt

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
    validPath (_, _, path, _, _) = sStartsWith path (C.pack "/")
    validType (t, _, _, _, _) = t `notElem` ['7', '2', '3', '8', 'T']

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

readFileU8 path = 
  openFile path ReadMode
  >>= \h -> hSetEncoding h char8
  >> hGetContents h

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

gopherGetRaw :: GopherUrl -> IO ByteString
gopherGetRaw url =
  getAddrInfo (Just addrInfoHints) (Just (host url)) (Just (port url))
  >>= return . addrAddress . head 
  >>= \addr -> socket AF_INET Stream 0 
    >>= \sock -> connect sock addr
      >> sendAll sock (C.pack $ appendCRLF (path url))
      >> recvAll sock

saveToFile :: ByteString -> FilePath -> IO ()
saveToFile bs path =
  createDirectoryIfMissing True (dropFileName path) >>
  withFile path WriteMode writeIt
  where
    writeIt handle = hPut handle bs >> hFlush handle

getAndSaveMenuFromNet :: GopherUrl -> IO [MenuLine]
getAndSaveMenuFromNet url = 
  getAndSaveToFile url 
  >>= return . parseMenu

getAndSaveToFile :: GopherUrl -> IO ByteString
getAndSaveToFile url = 
  gopherGetRaw url 
    >>= \bs -> saveToFile bs (urlToFilePath url)
    >> return bs

getMenuFromFileOrNet conf
  | (clobber conf)       = getAndSaveMenuFromNet
  | (not (clobber conf)) = getMenuMaybeFromFile

getMenuFromFile path =
  readFileU8 path 
  >>= return . parseMenu . C.pack

-- If file exists on disk, read it instead of accessing network
getMenuMaybeFromFile :: GopherUrl -> IO [MenuLine]
getMenuMaybeFromFile url = do
  pathExists <- doesPathExist path
  if pathExists 
    then getMenuFromFile path
    else getAndSaveMenuFromNet url
  where
    path = urlToFilePath url

{-----------------}
{------ Main -----}
{-----------------}

getRecursively :: GopherUrl -> Config -> IO [GopherUrl]
getRecursively url conf =
  crawlMenu url conf (maxDepth conf) Set.empty

crawlStatus :: Config -> Int -> GopherUrl -> String
crawlStatus conf depth refUrl = 
  let depthStr = "[" ++ (show $ (maxDepth conf) - depth) ++ "] " 
  in "(menu) " ++ depthStr ++ (urlToString refUrl)

crawlMenu :: GopherUrl -> Config -> Int -> Set.Set GopherUrl -> IO [GopherUrl]
crawlMenu refUrl conf depth history =
  putStrLn (crawlStatus conf depth refUrl) >>
  getMenuFromFileOrNet conf refUrl
  >>= return . filter (notInSet history) . map mlToUrl -- Convert lines to urls 
  >>= filterM (goodMenuUrl conf refUrl)                -- Filter urls by config 
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

getAndSaveToFilePrintStatus :: GopherUrl -> IO ()
getAndSaveToFilePrintStatus url =
  putStrFl ("(file) " ++ (urlToString url) ++ " ") >>
  getAndSaveToFile url >>= \bs -> 
  putStrLnFl ("(" ++ (show ((C.length bs) `div` 1000)) ++ "k)")

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
    mapM_ getAndSaveToFilePrintStatus [url]

getAndSaveUrls :: [GopherUrl]-> IO ()
getAndSaveUrls fileUrls = 
  putStrLn (":: Downloading " ++ (show (length fileUrls)) ++ " files ") >>
  mapM_ getAndSaveToFilePrintStatus fileUrls
