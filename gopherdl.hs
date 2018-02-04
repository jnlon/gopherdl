import Data.List
import Network.Socket
import Control.Exception
import qualified System.Environment as Env
import qualified Network.Socket.ByteString as BsNet
import qualified Data.ByteString.Char8 as Bs
import System.Console.GetOpt (OptDescr(Option),
                              ArgDescr(NoArg, ReqArg),
                              getOpt,
                              ArgOrder(RequireOrder))

putByteStrLn = Bs.putStrLn
sendAll = BsNet.sendAll
type ByteString = Bs.ByteString

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
 { recursive :: Bool
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

argDelay :: String -> Flag
argDelay delay =
  (Delay (read delay::Float))

argMaxDepth :: String -> Flag
argMaxDepth depth =
  (MaxDepth (read depth::Int))

options = 
  [ Option "r" [] (NoArg Recursive) "Enable recursive downloads"
  , Option "l" [] (ReqArg argMaxDepth "Int") "Maximum depth in recursive downloads"
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

gopherGet :: HostName -> String -> String -> IO ()
gopherGet host port path =
  getAddrInfo (Just addrInfoHints) (Just host) (Just port)
  >>= return . addrAddress . head 
  >>= \addr -> socket AF_INET Stream 0 
    >>= \sock -> connect sock addr
      >> sendAll sock (appendCRLF $ Bs.pack path)
      >> recvAll sock
      >>= putByteStrLn

main = do
  argv <- Env.getArgs
  (args, options) <- optionsFromFlags (getOpt RequireOrder options argv)
  putStrLn $ show options
  gopherGet "gopher.floodgap.com" "70" "/"
