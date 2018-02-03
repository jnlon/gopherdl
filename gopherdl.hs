import Network.Socket (connect, socket, Socket, SockAddr, 
                       Family(AF_INET), SocketType(Stream),
                       inet_addr, SockAddr(SockAddrInet))
--import Data.Bits.Utils (w82c)
import qualified Network.Socket.ByteString
import Control.Exception
import qualified Data.ByteString.Char8 as Bs
import Data.List
--import qualified Data.ByteString as Bs
import System.Console.GetOpt (OptDescr(Option),
                              ArgDescr(NoArg, ReqArg),
                              getOpt,
                              ArgOrder(RequireOrder))
import qualified System.Environment as Env

catchConnect ::  Socket -> SomeException -> IO String
catchConnect sock e =
  return $ "cantConnect: " ++ (show sock) ++ " \n >>> " ++ (show e)

putByteStrLn = Bs.putStrLn
recv = Network.Socket.ByteString.recv
sendAll = Network.Socket.ByteString.sendAll

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
  let has opt = opt `elem` options 
  in
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

--  putStrLn $ (unlines $ ((map show options) ++ (map ("[arg] "++) arguments)))
printStuff (options, arguments, errors) = 
  putStrLn $ (unlines $ ((map show options) ++ (map ("[arg] "++) arguments)))

recvAll :: Socket -> IO Bs.ByteString
recvAll sock = 
  recv sock 4096
  >>= (\bytes -> recvMore bytes)
  where 
    recvMore bytes =
      if (Bs.length bytes) == 0 
      then return bytes 
      else (recvAll sock) 
           >>= return . Bs.append bytes 

appendCRLF :: Bs.ByteString -> Bs.ByteString
appendCRLF bs =
  Bs.snoc (Bs.snoc bs '\r') '\n'

gopherGet :: SockAddr -> Bs.ByteString -> IO ()
gopherGet addr path =
  socket AF_INET Stream 0
  >>= (\s -> connect s addr >> return s)
  >>= (\s -> sendAll s (appendCRLF path) >> return s)
  >>= recvAll
  >>= putByteStrLn

main = do
  argv <- Env.getArgs
  (args, options) <- optionsFromFlags (getOpt RequireOrder options argv)
  putStrLn $ show options
  hostaddr <- inet_addr "66.166.122.165"
  gopherGet (SockAddrInet 70 hostaddr) (Bs.pack "/")
  -- putStrLn args
  {-socket AF_INET Stream 0
  >>= (\sock -> (catch (recv sock 10) (catchConnect sock)))
  >>= Prelude.putStrLn-}