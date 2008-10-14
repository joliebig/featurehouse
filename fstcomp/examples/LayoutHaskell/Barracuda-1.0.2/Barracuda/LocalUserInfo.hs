-- |
-- Maintainer: Stephan Friedrichs
--
-- A module to store all information about locally active users, i.e. users on this node, not the
-- remote users on the network.
module Barracuda.LocalUserInfo (
	LocalUserInfo,
	SingleUserInfo,
	users,
	channels,
	privateKey,
	handle,
	userSend
) where

import Barracuda.GUI.ServerInterface
import Data.Map as Map
import Data.Set as Set
import Network.AdHoc.Channel
import Network.AdHoc.UserID
import Network.GnuTLS.X509

-- | A 'SingleUserInfo' contains the joined channels (i.e. "Set"s of 'ChannelName' and 'ChannelID'),
-- | a private key and a reponse function for a local user.
type SingleUserInfo = (Set.Set (ChannelName, ChannelID), PrivateKey, ControlResponse -> IO ())

-- | A collection to store the 'SingleUserInfo' for locally active users (users at this node).
type LocalUserInfo = Map UserID SingleUserInfo

-- | Returns a list of all locally active users.
users :: LocalUserInfo -> Set UserID
users = keysSet

-- | Returns the channels a given user has joined.
channels :: UserID -> LocalUserInfo -> Maybe (Set.Set (ChannelName, ChannelID))
channels uid info = Map.lookup uid info >>= return.(\(x, _, _) -> x)

-- | Returns the private key of a given user.
privateKey :: UserID -> LocalUserInfo -> Maybe PrivateKey
privateKey uid info = Map.lookup uid info >>= return.(\(_, x, _) -> x)

-- | Returns a function to send a 'ControlResponse' to a given users GUI.
handle :: UserID -> LocalUserInfo -> Maybe (ControlResponse -> IO ())
handle uid info = Map.lookup uid info >>= return.(\(_, _, x) -> x)

-- | Directly sends a 'ControlResponse' to a given user. If the user is
--   not registered, @const (return ())@ is returned.
userSend :: UserID -> LocalUserInfo -> ControlResponse -> IO ()
userSend user info = maybe (const $ return ()) id $ handle user info

