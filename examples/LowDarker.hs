module LowDarker where

import HircFramework
import System.IO (Handle)
import Control.Concurrent (readChan)
import Data.List (intersperse, delete)

--
-- Example bot that can do some basic stuff
--

botNick = "lowdarker"
homeChannel = '#':botNick

hostInfo = ("localhost", 6667) -- host, port
botInfo = (botNick, "banned", "i am a bot") -- nick, user, description

pingCommand = "ping"
quitCommand = "quit"

main :: IO ()
main = startBot hostInfo botInfo onConnect

onConnect :: HircHandle -> IO ()
onConnect hh = join hh homeChannel >> readEvents hh [] >> return ()

readEvents :: HircHandle -> String -> IO String
readEvents hh@(_, _, ec) ns = readChan ec >>= onEvent hh ns >>= readEvents hh

-- _ -> NicksString -> _ -> IO NicksString
onEvent :: HircHandle -> String -> Event -> IO String
onEvent hh _ (ENames ch ts) = return . concat . intersperse ", " . delete botNick . map snd $ ts
onEvent hh ns (EJoin _ _) = names hh homeChannel >> return ns
onEvent hh ns (ENick _ _) = names hh homeChannel >> return ns
onEvent hh ns (EPart _ _ _) = names hh homeChannel >> return ns
onEvent hh ns (EKick _ _ _ _) = names hh homeChannel >> return ns
onEvent hh ns (EChannelMessage ch n m)
  | m == pingCommand = channelMessage hh homeChannel ns >> return ns
  | m == quitCommand = quit hh "" >> return ns
onEvent hh ns _ = return ns
