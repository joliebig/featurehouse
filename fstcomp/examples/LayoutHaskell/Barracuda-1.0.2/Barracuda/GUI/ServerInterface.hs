-- |
-- Maintainer: Stephan Friedrichs
--
-- This module represents the protocol used for communication between the gui
-- and the server components. It is the only link between the gui and the rest
-- of the application.
module Barracuda.GUI.ServerInterface (
	-- * Communication
	ControlMessage(..),
	ControlResponse(..),
	MessageType(..), -- reuse GTKs message type for warning levels
	-- * Interface
	GUI, InfraGUI
) where

import Data.Map
import Data.Set
import Data.Time
import Graphics.UI.Gtk.Windows.MessageDialog(MessageType(..))
import Network.AdHoc.Channel
import Network.AdHoc.Message
import Network.AdHoc.UserID
import Network.GnuTLS
import Network.Socket

-- | The messages sent from a graphical user interface to the Barracuda server.
--   Note that the first message to be sent, is 'SetUser'.
data ControlMessage
	= SetUser
		{ suUserID :: UserID
		, suCert   :: Certificate
		, suKey    :: PrivateKey
		} -- ^ Sets information about the user using the GUI.
	| WantJoin
		{ joinChannelName :: ChannelName
		, joinChannelID   :: ChannelID
		} -- ^ The local user wants to join a channel.
	| WantLeave
		{ leaveChannelName :: ChannelName
		, leaveChannelID   :: ChannelID
		} -- ^ The local user wants to leave a channel.
	| SendMsg
		{ sendChannelName :: ChannelName
		, sendChannelID   :: ChannelID
		, sendMessage     :: String
		, sendAttachments :: [Attachment]
		} -- ^ The local user sent a message to a channel.
	| CreateChannel
		{ createName        :: ChannelName
		, createDescription :: String
		, createInvite      :: Maybe (Set UserID) -- ^ 'Nothing' for a public channel, 'Just' inviations for a private one.
		} -- ^ The local user wants to create a new channel.
	| Authorize
		{ authUser        :: UserID
		, authChannelName :: ChannelName
		, authChannelID   :: ChannelID
		} -- ^ The local user authorized another user to join a private channel.
	| CMClose -- ^ The graphical user interface has been shut down. This message must
	          --   be the last thing, a GUI sends, even if it had been closed by
		  --   'CRClose' before.
	deriving (Show)

-- | The messages sent from the Barracuda server to the graphical user interface.
data ControlResponse
	= AllChans
		{ chansMap :: Map (ChannelName, ChannelID) (String, Bool, Set UserID)
		} -- ^ A complete list of channels and the users inside each of them.
	| Receive
		{ receiveChannelName :: ChannelName
		, receiveChannelID   :: ChannelID
		, receiveSender      :: Maybe UserID
		, receiveMessage     :: String
		, receiveAttachments :: [Attachment]
		, receiveTimestamp   :: UTCTime
		, receiveDelayed     :: Bool
		} -- ^ The given message has been sent to the indicated channel.
	| WantsAuth
		{ wantsAUser        :: UserID
		, wantsAChannelName :: ChannelName
		, wantsAChannelID   :: ChannelID
		} -- ^ The indicated user requested authorization for the given private channel.
	| ErrGeneral
		{ errGenLevel   :: MessageType
		, errGenTitle   :: String
		, errGenMessage :: String

		} -- ^ An error occured, that has not been further specified.
	| ErrNotDelivered
		{ errDelUsers       :: [UserID]
		, errDelChannelName :: ChannelName
		, errDelChannelID   :: ChannelID
		, errDelMessage     :: String
		, errDelAttachments :: [Attachment]
		, errDelTime        :: UTCTime
		} -- ^ A message could not be delivered to some users.
	| CRClose -- ^ The user interface shall shut down.
	deriving (Show)

instance Show MessageType where
	show MessageInfo      = "MessageInfo"
	show MessageWarning   = "MessageWarning"
	show MessageQuestion  = "MessageQuestion"
	show MessageError     = "MessageError"
	show MessageOther     = "MessageOther"

-- | A function every graphical user interface for Barracuda has to provide.
type GUI = (ControlMessage -> IO ())     -- ^ A function to send 'ControlMessage's to the server.
	-> IO (ControlResponse -> IO ()) -- ^ A function to send 'ControlResponse's to the GUI.

-- | A function type every infrastructure-mode configuration GUI must provide.
type InfraGUI = (Maybe (Set SockAddr) -> IO ()) -- ^ A function the infrastructure GUI can call to
	                                        --   change the infrastructural data.
	-> Maybe (Set SockAddr)                 -- ^ The initial configuration ('Nothing' means: inframode
	                                        --   turned off, otherwise only the listed 'SockAddr's
	                                        --   are processed (see 'Barracuda.Distributor.DistributorMsg').
	-> IO ()

