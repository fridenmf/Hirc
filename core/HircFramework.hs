module HircFramework(
  InternalChan,
  ExternalChan,
  Event(
    EReplyYourHost,
    EReplyCreated,
    EReplyMyInfo,
    EPing,
    EUserMode,
    EChannelMode,
    EChannelUserMode,
    EChannelMessage,
    EPrivateMessage,
    EMotd,
    EPart,
    EJoin,
    EInvite,
    EKick,
    ENick,
    ETopic,
    EChannelNotice,
    EPrivateNotice,
    ENames,
    EAway
  ),
  HircHandle,
  BotFunc,
  startBot,
  logMessage,
  privateMessage,
  channelMessage,
  privateNotice,
  channelNotice,
  join,
  nick,
  user,
  pong,
  whois,
  whowas,
  kick,
  away,
  unAway,
  rehash,
  users,
  part,
  userMode,
  userChannelMode,
  channelMode,
  getTopic,
  setTopic,
  raw,
  names,
  quit,
  invite
) where

import DataTypes

import Network
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Monad (liftM, liftM2)
import Control.Concurrent
import Data.List
import Data.Maybe

-- | This function starts the bot
startBot :: ServerInfo -> BotInfo -> BotFunc -> IO ()
startBot (hst, prt) botInfo@(nck, usr, dsc) botFunc = withSocketsDo $ do

    handle <- connectTo hst (PortNumber prt)
    hSetBuffering handle LineBuffering

    -- Internal channel for writing/reading data to sockets
    ic <- newChan

    -- External channel for reader to send events to framework and to tell writer to send data
    ec <- newChan

    -- Bundle the handle, internal and external channels into a HircHandle
    let hh = (handle, ic, ec)

    -- Set user and nick
    user hh usr dsc
    nick hh nck

    -- Thread for logging
    spawnThread hh $ writerThreadFunc

    -- Thread for reading from server
    spawnThread hh $ flip readerThreadFunc botFunc

    return ()

logMessage :: HircHandle -> String -> IO ()
logMessage (_,ic,_) s = writeChan ic (Log, s)

send :: HircHandle -> String -> IO ()
send hh@(handle,_,_) message = do
  logMessage hh $ "--> " ++ message
  hPutStrLn handle message

privateMessage :: HircHandle -> String -> String -> IO ()
privateMessage hh nick msg = send hh $ "PRIVMSG " ++ nick ++ " :" ++ msg

channelMessage :: HircHandle -> String -> String -> IO ()
channelMessage hh channel message = privateMessage hh channel message

privateNotice :: HircHandle -> String -> String -> IO ()
privateNotice hh channel message = send hh $ "NOTICE " ++ channel ++ " :" ++ message

channelNotice :: HircHandle -> String -> String -> IO ()
channelNotice hh channel message = privateNotice hh channel message

join :: HircHandle -> String -> IO ()
join hh channel = send hh $ "JOIN " ++ channel

nick :: HircHandle -> String -> IO ()
nick hh newNick = send hh $ "NICK " ++ newNick

user :: HircHandle -> String -> String -> IO ()
user hh newUser newDescription = send hh $ "USER " ++ newUser ++ " 8 * : " ++ newDescription

pong :: HircHandle -> String -> IO ()
pong hh message = send hh $ "PONG " ++ message

whois :: HircHandle -> String -> IO ()
whois hh nick = send hh $ "WHOIS " ++ nick

whowas :: HircHandle -> String -> IO ()
whowas hh nick = send hh $ "WHOWAS " ++ nick

kick :: HircHandle -> String -> String -> String -> IO ()
kick hh channel nick reason = send hh $ "KICK " ++ channel ++ " " ++ nick ++ " :" ++ reason

away :: HircHandle -> String -> IO ()
away hh reason = send hh $ "AWAY " ++ reason

unAway :: HircHandle -> IO ()
unAway hh = send hh $ "AWAY"

rehash :: HircHandle -> IO ()
rehash hh = send hh "REHASH"

users :: HircHandle -> IO ()
users hh = send hh "USERS"

part :: HircHandle -> String -> String -> IO ()
part hh channel message = send hh $ "PART " ++ channel ++ " " ++ message

userMode :: HircHandle -> String -> String -> IO ()
userMode hh nick modes = send hh $ "MODE " ++ nick ++ " " ++ modes

userChannelMode :: HircHandle -> String -> String -> String -> IO ()
userChannelMode hh channel modes nicks = send hh $ "MODE " ++ channel ++ " " ++ modes ++ " " ++ nicks

channelMode :: HircHandle -> String -> String -> IO ()
channelMode hh channel modes = send hh $ "MODE " ++ channel ++ " " ++ modes

getTopic :: HircHandle -> String -> IO ()
getTopic hh channel = send hh $ "TOPIC " ++ channel

setTopic :: HircHandle -> String -> String -> IO ()
setTopic hh channel topic = send hh $ "TOPIC " ++ channel ++ " :" ++ topic

raw :: HircHandle -> String -> IO ()
raw hh string = send hh string

names :: HircHandle -> String -> IO ()
names hh channel = send hh $ "NAMES " ++ channel

quit :: HircHandle -> String -> IO ()
quit hh message = send hh $ "QUIT " ++ message

invite :: HircHandle -> String -> String -> IO ()
invite hh channel nick = send hh $ "INVITE " ++ nick ++ " " ++ channel

-- Thread for logging asynchronously
writerThreadFunc :: HircHandle -> IO ()
writerThreadFunc (h, ic, ec) = do
  (g, s) <- readChan ic
  case g of
    Log -> putStrLn s
    _   -> putStrLn "Unknown group in writerThreadFunc"
  writerThreadFunc (h, ic, ec)

readerThreadFunc :: HircHandle -> BotFunc -> IO ()
readerThreadFunc hh@(handle, _, _) botFunc = hGetLine handle >>= handleLine hh botFunc >> readerThreadFunc hh botFunc

-- Handles all interpretation of server messages
handleLine :: HircHandle -> BotFunc -> String -> IO ()
handleLine hh botFunc line = do
  logMessage hh $ "<-- " ++ line
  let ws = words line
  case head . tail $ ws of
    _ | head ws == "PING" -> handlePing hh ws
      | length ws < 3 -> handleUnparsed hh line
      | isPrefixOf "ERROR :Closing Link: " line -> handleConnectionClosed hh ws
    "001" -> handleConnect hh botFunc line
    "002" -> handleReplyYourHost hh line
    "003" -> handleReplyCreated hh line
    "004" -> handleReplyMyInfo hh line
    "251" -> handleReplyLUserClient hh line
    "252" -> handleReplyLUserOp hh line
    "254" -> handleReplyLUserChannels hh line
    "255" -> handleReplyLUserMe hh line
    "301" -> handleAwayReply hh ws
    "305" -> handleUnAway hh
    "306" -> handleAway hh
    "311" -> handleWhoisReply hh ws
    "353" -> handleNames hh ws
    "366" -> handleEndOfNames hh line
    "375" -> handleMotd hh []
    "401" -> handleNoSuchNickError hh ws
    "404" -> handleMessageError hh ws
    "433" -> handleNickAlreadyTakenError hh ws
    "471" -> handleFailToJoin hh ws
    "473" -> handleFailToJoin hh ws
    "474" -> handleFailToJoin hh ws
    "475" -> handleFailToJoin hh ws
    "PRIVMSG" -> handleMessage hh ws
    "JOIN"    -> handleJoin hh ws
    "KICK"    -> handleKick hh ws
    "PART"    -> handlePart hh ws
    "INVITE"  -> handleInvite hh ws
    "TOPIC"   -> handleTopic hh ws
    "NOTICE"  -> handleNotice hh ws
    "NICK"    -> handleNick hh ws
    "MODE"    ->
      if isPrefixOf "#" $ ws !! 2
        then if length ws == 4
          -- channel modes (ex: i - invite only)
          then handleChannelMode hh ws
          -- user modes for a specific channel (ex: v - voice)
          else handleChannelUserMode hh ws
        -- user modes(ex: r - identified, w - wallop)
        else handleUserMode hh ws
    otherwise -> handleUnparsed hh line
  return ()

handleUnparsed :: HircHandle -> String -> IO ()
handleUnparsed hh line = logMessage hh $ "Unparsed: " ++ line

handleReplyYourHost :: HircHandle -> String -> IO ()
handleReplyYourHost hh line = sendEvent hh $ EReplyYourHost serverName version where
  serverName = init . head . drop 6 . words $ line
  version = last . words $ line

handleMessage :: HircHandle -> [String] -> IO ()
handleMessage hh ws =
  let sender = takeWhile (not . (==) '!') . tail . head $ ws
      target = ws !! 2
      msg  = tail . unwords . drop 3 $ ws
  in if isChannel target
    then sendEvent hh $ EChannelMessage target sender msg
    else sendEvent hh $ EPrivateMessage sender msg

handleJoin :: HircHandle -> [String] -> IO ()
handleJoin hh ws = sendEvent hh $ EJoin channel nick where
  nick = takeWhile (not . (==) '!') . tail . head $ ws
  channel = tail . (!!) ws $ 2

handlePart :: HircHandle -> [String] -> IO ()
handlePart hh ws = sendEvent hh $ EPart channel nick msg where
  nick = takeWhile (not . (==) '!') . tail . head $ ws
  channel = ws !! 2
  msg = drop 1 . unwords . drop 3 $ ws

handleInvite :: HircHandle -> [String] -> IO ()
handleInvite hh ws = sendEvent hh $ EInvite channel nick where
  nick = takeWhile (not . (==) '!') . tail . head $ ws
  channel = tail . (!!) ws $ 2

handleKick :: HircHandle -> [String] -> IO ()
handleKick hh ws = sendEvent hh $ EKick channel kicker kicked msg where
  kicker = takeWhile (not . (==) '!') . tail . head $ ws
  channel = ws !! 2
  kicked = ws !! 3
  msg = tail . unwords . drop 4 $ ws

handleTopic :: HircHandle -> [String] -> IO ()
handleTopic hh ws = sendEvent hh $ ETopic channel nick msg where
  nick = takeWhile (not . (==) '!') . tail . head $ ws
  channel = ws !! 2
  msg = tail . unwords . drop 3 $ ws

handleNotice :: HircHandle -> [String] -> IO ()
handleNotice hh ws =
  let nick = takeWhile (not . (==) '!') . tail . head $ ws
      target = ws !! 2
      msg = tail . unwords . drop 3 $ ws
  in if isChannel target
    then sendEvent hh $ EChannelNotice target nick msg
    else sendEvent hh $ EPrivateNotice nick msg

handleChannelMode :: HircHandle -> [String] -> IO ()
handleChannelMode hh ws = sendEvent hh $ EChannelMode channel nick modes where
  nick = takeWhile (not . (==) '!') . tail . head $ ws
  channel = ws !! 2
  modes = ws !! 3

handleChannelUserMode :: HircHandle -> [String] -> IO ()
handleChannelUserMode hh ws = sendEvent hh $ EChannelUserMode channel nick $ parseModeTuple modes targets where
  nick = takeWhile (not . (==) '!') . tail . head $ ws
  channel = ws !! 2
  modes = ws !! 3
  targets = drop 4 ws

handleUserMode :: HircHandle -> [String] -> IO ()
handleUserMode hh ws = sendEvent hh $ EUserMode nick target modes where
  nick = takeWhile (not . (==) '!') . tail . head $ ws
  target = ws !! 2
  modes = ws !! 3

handleNames :: HircHandle -> [String] -> IO ()
handleNames hh ws = sendEvent hh $ ENames channel $ map createTup . words . tail . unwords . drop 5 $ ws where
  channel = ws !! 4
  createTup :: String -> (String, String)
  createTup ('~':s) = ("~", s)
  createTup ('@':s) = ("@", s)
  createTup ('+':s) = ("+", s)
  createTup s = ([], s)

handleMotd :: HircHandle -> [String] -> IO ()
handleMotd hh@(handle, ic, ec) motd = do
  line <- hGetLine handle
  let ws = words line
  case ws !! 1 of
    "372" -> handleMotd hh (motd++[unwords . drop 2 $ ws])
    "376" -> sendEvent hh $ EMotd motd
    _     -> do
      logMessage hh $ "Unhandled motd line: \"" ++ line ++ "\""
      handleMotd hh motd

handlePing :: HircHandle -> [String] -> IO ()
handlePing hh@(handle, ic, ec) ws = do
  pong hh "bap"
  writeChan ec $ EPing $ unwords $ tail ws

handleNick :: HircHandle -> [String] -> IO ()
handleNick hh@(handle, ic, ec) ws = sendEvent hh $ ENick nickFrom nickTo where
  nickFrom = takeWhile (not . (==) '!') . tail . head $ ws
  nickTo = last . tail $ ws

handleConnect :: HircHandle -> BotFunc -> String -> IO ()
handleConnect hh botFunc line = do
  logMessage hh "Connected!"
  spawnThread hh $ botFunc
  return ()

handleUnAway :: HircHandle -> IO ()
handleUnAway hh = sendEvent hh EUnAway

handleAway :: HircHandle -> IO ()
handleAway hh = sendEvent hh EAway

handleConnectionClosed :: HircHandle -> [String] -> IO ()
handleConnectionClosed hh ws = handleUnparsed hh $ unwords ws

-- | Pairs prefixes with nicks
parseModeTuple :: String -> [String] -> [(String, String)]
parseModeTuple = parseModeTuple' True where
  parseModeTuple' :: Bool -> String -> [String] -> [(String, String)]
  parseModeTuple' _ [] _ = []
  parseModeTuple' _ _ [] = []
  parseModeTuple' _ ('+':ps) ns = parseModeTuple' True ps ns
  parseModeTuple' _ ('-':ps) ns = parseModeTuple' False ps ns
  parseModeTuple' True (p:ps) (n:ns) = (:) ('+':[p], n) $ parseModeTuple' True ps ns
  parseModeTuple' False (p:ps) (n:ns) = (:) ('-':[p], n) $ parseModeTuple' False ps ns

isChannel :: String -> Bool
isChannel = (==) '#' . head

sendEvent :: HircHandle -> Event -> IO ()
sendEvent (_, _, ec) event = writeChan ec event

spawnThread :: HircHandle -> (HircHandle -> IO ()) -> IO ThreadId
spawnThread (h, ic, ec) func = do
  ic' <- dupChan ic
  ec' <- dupChan ec
  forkIO $ func (h, ic', ec')

--
-- TODO implement these
--

handleEndOfNames :: HircHandle -> String -> IO ()
handleEndOfNames hh line = handleUnparsed hh line

handleReplyCreated :: HircHandle -> String -> IO ()
handleReplyCreated hh line = handleUnparsed hh line

handleReplyMyInfo :: HircHandle -> String -> IO ()
handleReplyMyInfo hh line = handleUnparsed hh line

handleReplyLUserClient :: HircHandle -> String -> IO ()
handleReplyLUserClient hh line = handleUnparsed hh line

handleReplyLUserOp :: HircHandle -> String -> IO ()
handleReplyLUserOp hh line = handleUnparsed hh line

handleReplyLUserChannels :: HircHandle -> String -> IO ()
handleReplyLUserChannels hh line = handleUnparsed hh line

handleReplyLUserMe :: HircHandle -> String -> IO ()
handleReplyLUserMe hh line = handleUnparsed hh line

handleFailToJoin :: HircHandle -> [String] -> IO ()
handleFailToJoin hh ws = handleUnparsed hh $ unwords ws

handleMessageError :: HircHandle -> [String] -> IO ()
handleMessageError hh ws = handleUnparsed hh $ unwords ws

handleNoSuchNickError :: HircHandle -> [String] -> IO ()
handleNoSuchNickError hh ws = handleUnparsed hh $ unwords ws

handleAwayReply :: HircHandle -> [String] -> IO ()
handleAwayReply hh ws = handleUnparsed hh $ unwords ws

handleNickAlreadyTakenError :: HircHandle -> [String] -> IO ()
handleNickAlreadyTakenError hh ws = handleUnparsed hh $ unwords ws

handleWhoisReply :: HircHandle -> [String] -> IO ()
handleWhoisReply hh ws = handleUnparsed hh $ unwords ws
