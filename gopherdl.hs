import Data.List
import Data.Maybe
import Data.Strings
import Network.Socket
import Control.Exception
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
type MenuEntity = (Char, ByteString, ByteString, ByteString, ByteString)
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
    | Delay Float
    | FlagDebug deriving (Show, Eq)

data Options = Options
 {  recursive :: Bool
  , maxDepth :: Int
  , spanHosts :: Bool
  , help :: Bool
  , clobber :: Bool
  , onlyMenus :: Bool
  , noMenus :: Bool
  , ascendParent :: Bool
  , delay :: Float
  , flagDebug :: Bool 
  } deriving (Show)

{--------------------------}
{------ Argv Parsing ------}
{--------------------------}

debugLog :: Show a => Options -> a -> IO a
debugLog opts a =
  (if flagDebug opts
    then putStrLn $ ("DEBUG: " ++ show a)
    else return ()) >> return a

argDelay :: String -> Flag
argDelay delay =
  (Delay (read delay::Float))

argMaxDepth :: String -> Flag
argMaxDepth depth =
  (MaxDepth (read depth::Int))

optionSpec = 
  [ Option "r" [] (NoArg Recursive) "Enable recursive downloads"
  , Option "l" [] (ReqArg argMaxDepth "int") "Maximum depth in recursive downloads"
  , Option "s" [] (NoArg SpanHosts) "Span hosts on recursive downloads"
  , Option "h" [] (NoArg Help) "Show this help"
  , Option "c" [] (NoArg Clobber) "Enable file clobbering (overwrite existing)"
  , Option "m" [] (NoArg OnlyMenus) "Only download gopher menus"
  , Option "n" [] (NoArg NoMenus) "Never download gopher menus"
  , Option "p" [] (NoArg AscendParent) "Allow ascension to the parent directories"
  , Option "w" [] (ReqArg argDelay "Seconds") "Delay between downloads"
  , Option "d" [] (NoArg FlagDebug) "Enable debug messages" ]

isMaxDepth (MaxDepth _) = True
isMaxDepth otherwise = False

isDelay (Delay _) = True
isDelay otherwise = False

findMaxDepth def opts =
  case (find isMaxDepth opts) of
    Just (MaxDepth d) -> d
    _ -> def

findDelay def opts =
  case (find isDelay opts) of
    Just (Delay d) -> d
    _ -> def

optionsFromFlags :: ([Flag], [String], [String]) -> IO ([String], Options)
optionsFromFlags (options, arguments, errors) = 
  return ( arguments, 
           Options { recursive = has Recursive
                   , maxDepth = findMaxDepth 99 options
                   , spanHosts = has SpanHosts
                   , help = has Help
                   , clobber = has Clobber
                   , onlyMenus = has OnlyMenus
                   , noMenus = has NoMenus
                   , ascendParent = has AscendParent
                   , delay = findDelay 0.0 options
                   , flagDebug = has FlagDebug })
  where 
    has opt = opt `elem` options 

{------------------------}
{----- Menu Parsing -----}
{------------------------}

menuEntToUrl :: MenuEntity -> GopherUrl
menuEntToUrl (_, _, path, host, port) =
  (C.unpack host, C.unpack path, C.unpack port)

urlToString :: GopherUrl -> String
urlToString (host, path, port) =
  "gopher://" ++ host ++ ":" ++ port ++ path

validPath :: Maybe MenuEntity -> Bool
validPath (Just (_, _, path, _, _)) = 
  (sLen path) /= 0 &&
  not (sStartsWith (strToUpper path) (C.pack "URL:"))

parseMenu :: ByteString -> [MenuEntity]
parseMenu rawMenu = 
  map fromJust $ filter valid $ map parseMenuLine lines
  where 
    lines = C.lines rawMenu
    valid line = isJust line && validPath line

parseMenuLine :: ByteString -> Maybe MenuEntity
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

{----------------------}
{------ IO & main -----}
{----------------------}

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

gopherGetRaw :: GopherUrl -> IO ByteString
gopherGetRaw (host, path, port) =
  getAddrInfo (Just addrInfoHints) (Just host) (Just port)
  >>= return . addrAddress . head 
  >>= \addr -> socket AF_INET Stream 0 
    >>= \sock -> connect sock addr
      >> sendAll sock (appendCRLF $ C.pack path)
      >> recvAll sock

sanitizePath path = 
  filter nonEmptyString $ strSplitAll "/" path
  where
    nonEmptyString = (/=) 0 . sLen

toFilePath :: MenuEntity -> FilePath
toFilePath (_type, text, path, host, port) =
  joinPath $ 
    [C.unpack host] 
    ++ (map C.unpack (sanitizePath path))
    ++ (if _type == '1' then ["gophermap"] else [])

main =
  Env.getArgs 
  >>= \argv -> optionsFromFlags (getOpt RequireOrder optionSpec argv)
  >>= main'

main' :: ([String], Options) -> IO ()
main' (args, opts) =
  if (help opts) then
    putStr $ usageInfo "Usage:" optionSpec
  else if (recursive opts) then
    putStrLn "Recursive!" >>
    gopherGetRaw ("gopher.floodgap.com", "/", "70")
    >>= return . map toFilePath . parseMenu
    >>= debugLog opts >> return ()
--    >>= 
  else
    gopherGetRaw ("gopher.floodgap.com", "/", "70")
    >>= debugLog opts >> return ()
