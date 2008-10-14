-- |
-- Maintainer: Martin Wegner
--
-- A data structure supporting the delaying of actions until the public key of some
-- user could be retrieved over the network.

module Barracuda.PendingKey
	( PendingKey
	, PendingKeyMessage
	, empty
	, insert
	, purge
	)
	where

import qualified Data.Map as Map
import Data.Time.Clock
import Network.GnuTLS.X509

import Network.AdHoc.Channel
import Network.AdHoc.Message
import Network.AdHoc.Signature
import Network.AdHoc.UserID

-- | A message waiting for a shared key of a private channel.
--   Can be either a local message waiting to be encrypted and sent over the network
--   or a foreign message waiting to be decrypted and sent to local users.
type PendingKeyMessage = Either (UserID, PrivateKey, TargetContent) (UserID, TargetContent, UTCTime, Bool)
-- | Stores 'PendingKeyMessage's on a per-channel basis.
newtype PendingKey = PendingKey (Map.Map (ChannelName, ChannelID) [ PendingKeyMessage ]) deriving (Show)

-- | Builds an empty 'PendingKey' structure.
empty :: PendingKey
empty = PendingKey Map.empty

-- | Inserts a 'PendingKeyMessage' for the specified channel into a 'PendingKey'.
insert :: ChannelName -- ^ 'ChannelName' of the channel the shared key is missing for.
	-> ChannelID -- ^ 'ChannelID' of the channel.
	-> PendingKeyMessage -- ^ The message that cannot be de- or encrypted.
	-> PendingKey
	-> PendingKey
insert cname cid msg (PendingKey pk)
	= let msgs = case Map.lookup (cname, cid) pk of
		Just rmsgs -> (msg:rmsgs)
		Nothing -> [ msg ]
	in PendingKey (Map.insert (cname, cid) msgs pk)

-- | Returns all 'PendingKeyMessage's that are waiting for the key of the given channel.
purge :: ChannelName -- ^ 'ChannelName' of channel to check for pending messages.
	-> ChannelID -- ^ 'ChannelID' of the channel.
	-> PendingKey
	-> (PendingKey, Maybe [ PendingKeyMessage ]) -- ^ Returns a list of 'PendingKeyMessage's or 'Nothing' if no messages were stored.
purge cname cid (PendingKey pk) = (PendingKey (Map.delete (cname, cid) pk), Map.lookup (cname, cid) pk)

