import Data.List
import Data.Strings
import Data.Maybe
import Network.Socket
import Control.Exception
import qualified System.Environment as Env
import qualified Network.Socket.ByteString as BsNet
import qualified Data.ByteString.Char8 as Bs
import System.Console.GetOpt (OptDescr(Option),
                              ArgDescr(NoArg, ReqArg),
                              getOpt,
                              ArgOrder(RequireOrder),
                              usageInfo)

putByteStrLn = Bs.putStrLn
sendAll = BsNet.sendAll
type ByteString = Bs.ByteString

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

{- Argv Parsing -}

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

{- Menu Parsing -}

menuEntToUrl :: MenuEntity -> GopherUrl
menuEntToUrl (_, _, path, host, port) =
  (Bs.unpack host, Bs.unpack path, Bs.unpack port)

urlToString :: GopherUrl -> String
urlToString (host, path, port) =
  "gopher://" ++ host ++ ":" ++ port ++ path

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

hasNonEmptyPath :: Maybe MenuEntity -> Bool
hasNonEmptyPath (Just (_, _, path, _, _)) = (Bs.length path) /= 0
hasNonEmptyPath _ = False

parseMenu :: ByteString -> [MenuEntity]
parseMenu rawMenu = 
  map fromJust $ filter valid $ map parseMenuLine $ Bs.lines rawMenu
  where 
    valid line = isJust line && hasNonEmptyPath line

{- Network IO -}

recvAll :: Socket -> IO ByteString
recvAll sock = 
  BsNet.recv sock 4096 >>= recvMore
  where 
    recvMore bytes =
      if (Bs.length bytes) == 0 
        then return bytes 
        else recvAll sock >>= return . Bs.append bytes 

appendCRLF :: ByteString -> ByteString
appendCRLF bs = Bs.append bs $ Bs.pack "\r\n"

addrInfoHints :: AddrInfo
addrInfoHints = defaultHints { addrSocketType = Stream }

gopherGetRaw :: GopherUrl -> IO ByteString
gopherGetRaw (host, path, port) =
  getAddrInfo (Just addrInfoHints) (Just host) (Just port)
  >>= return . addrAddress . head 
  >>= \addr -> socket AF_INET Stream 0 
    >>= \sock -> connect sock addr
      >> sendAll sock (appendCRLF $ Bs.pack path)
      >> recvAll sock

main = do
  Env.getArgs 
  >>= \argv -> optionsFromFlags (getOpt RequireOrder optionSpec argv)
  >>= main'
--  gopherGetRaw ("gopher.floodgap.com", "/", "70")
--  >>= putStrLn . show . parseMenu

main' :: ([String], Options) -> IO ()
main' (args, opts) =
  if (help opts) then
    putStr $ usageInfo "Usage:" optionSpec
  else
    gopherGetRaw ("gopher.floodgap.com", "/", "70")
    >>= putStrLn . show . parseMenu
