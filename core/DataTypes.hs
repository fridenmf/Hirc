module DataTypes where

import Network
import Control.Concurrent
import System.IO (Handle)

-- |Events from the server. Patternmatch this in your 'BotThread'.
data Event =
  EReplyYourHost ServerName Version |
  EReplyCreated Date |
  EReplyMyInfo ServerName Version UserModes ChannelModes |
  EPing Message |
  EPrivateMessage Nick Message |
  EChannelMessage Channel Nick Message |
  EPart Channel Nick Message |
  EJoin Channel Nick |
  EInvite Channel Nick |
  EKick Channel Nick Nick Message |
  ETopic Channel Nick Message |
  EPrivateNotice Nick Message |
  EChannelNotice Channel Nick Message |
  EUserMode Nick Nick Modes |
  EChannelMode Channel Nick Modes |
  EChannelUserMode Channel Nick [(Prefix, Nick)] |
  ENames Channel [(Prefix, Nick)] |
  EMotd [String] |
  ENick Nick Nick |
  EAway |
  EUnAway |
  EConnectionClosed

-- TODO hide this data type
-- |Used internally in the framework
data Target =
  Log |
  Writer |
  Reader |
  Framework

-- |Used for internal commnands like Logging ('Log').
--  Don't use this directly, pass it as an argument.
type InternalChan = Chan (Target, String)

-- |Used for external commands, like sending a private message ('PvtMsg').
--  Use helper functions instead of using this directly, for example 'privateMessage'.
type ExternalChan = Chan (Event)

-- |Wraps three things required all over Hirc to simplify usage
type HircHandle   = (Handle, InternalChan, ExternalChan)

-- |Will be called when the bot has connected to the server
type BotFunc = HircHandle -> IO ()

type ServerInfo   = (Host, PortNumber)
type BotInfo      = (Nick, User, Description)

type IsAway = Bool

type Channel = String
type Modes   = String
type Message = String
type Prefix  = String

type User         = String
type Nicks        = [Nick]
type Nick         = String
type Host         = String
type Description  = String

-- Replies from server
type ServerName   = String
type Version      = String
type Date         = String
type UserModes    = String
type ChannelModes = String

type NumberOfUsers     = Integer
type NumberOfServices  = Integer
type NumberOfServers   = Integer
type NumberOfOperators = Integer
type NumberOfClients   = Integer
