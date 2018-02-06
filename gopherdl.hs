import Data.List
import Data.Maybe
import Data.Strings
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


{- TODO
  - Function to read from socket while writing to a file? 
    - Will we run out of ram downloading a large file, or does lazyness help?
-}

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

data CrawlNode = 
    FileNode GopherUrl
  | MenuNode GopherUrl
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

sanitizePath path = 
  let nonEmptyString = (/=) 0 . sLen in
  filter nonEmptyString $ strSplitAll "/" path

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

{------------------------}
{----- Menu Parsing -----}
{------------------------}

menuLineToUrl :: MenuLine -> GopherUrl
menuLineToUrl (_, _, path, host, port) =
  (C.unpack host, C.unpack path, C.unpack port)

urlToString :: GopherUrl -> String
urlToString (host, path, port) =
  "gopher://" ++ host ++ ":" ++ port ++ path

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

saveFromMenuLine :: ByteString -> MenuLine -> IO ()
saveFromMenuLine bs ml = 
  save bs (menuLineToUrl ml) (lineIsMenu ml)

buildNodeTree :: GopherUrl -> [CrawlNode] -> IO [CrawlNode]
buildNodeTree url history =
  gopherGetMenu url 
  >>= debugLog
  >>= mapM nodifyLine -- Correct
  >>= return . filter notInHistory
  >>= mapM (getNodes history)
  >>= return . concat
  where
    notInHistory e = e `notElem` history

getNodes :: [CrawlNode] -> CrawlNode -> IO [CrawlNode]
getNodes history node = 
  case node of
    FileNode url -> return [node]
    MenuNode url -> fmap ((:) node) (buildNodeTree url (node : history))

nodifyLine :: MenuLine -> IO CrawlNode
nodifyLine line = 
  if (lineIsMenu line) 
    then return $ MenuNode (menuLineToUrl line)
    else return $ FileNode (menuLineToUrl line)
    
gopherGetMenu :: GopherUrl -> IO [MenuLine]
gopherGetMenu url = 
  debugLog ("gopherGetMenu: " ++ (urlToString url)) >>
  gopherGetRaw url >>= return . parseMenu

{-----------------}
{------ Main -----}
{-----------------}

main =
  Env.getArgs 
  >>= main' . configFromGetOpt . parseWithGetOpt

main' :: ([String], Config) -> IO ()
main' (args, conf) =
  if (help conf) || (length args) == 0 
    then putStr $ usageInfo "gopherdl [options] [urls]" optionSpec
  else if (recursive conf) then
    putStrLn "Recursive!" >>
    buildNodeTree ("gopher.floodgap.com", "/", "70") []
    >>= putStrLn . show
  else
    gopherGetMenu ("gopher.floodgap.com", "/", "70")
    >>= debugLog >> return ()
