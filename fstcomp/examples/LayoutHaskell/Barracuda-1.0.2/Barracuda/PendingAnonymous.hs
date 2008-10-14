-- |
-- Maintainer: Martin Wegner
-- 
-- A data structure used to observe the spreading of locally initiated anonymous messages.

module Barracuda.PendingAnonymous
	( PendingAnonymousMessage(..)
	, PendingAnonymous
	, empty
	, insert
	, purgeNotPosted
	, purgeZeroTTL
	, posted
	)
	where

import qualified Data.Map as Map
import Data.Time.Calendar
import Data.Time.Clock

import Network.AdHoc.Message
import Network.AdHoc.UserID

-- | An anonymous message whose posting in the anonymous channel needs to be monitored.
data PendingAnonymousMessage = PendingAnonymousMessage
	{ ttl         :: TTL
	, sender      :: UserID
	, text        :: String
	, attachments :: [ Attachment ]
	, time        :: UTCTime
	}
-- | 'PendingAnonymous' maps the identifiers of an anonymous message, the text and the time, to the rest
--   of the attributes and a timestamp when the message was added. This way it allows the monitoring of
--   the posting of anonymous messages and provides the ability to re-post messages that (might) have been
--   lost during the path obscuring.
newtype PendingAnonymous = PendingAnonymous (Map.Map (String, UTCTime) (UTCTime, TTL, UserID, [ Attachment ])) deriving (Show)

-- | Builds an empty 'PendingAnonymous' structure.
empty :: PendingAnonymous
empty = PendingAnonymous Map.empty

-- | Inserts the given message into the 'PendingAnonymous' structure.
insert :: UTCTime -- ^ Timestamp of now.
	-> PendingAnonymousMessage -- ^ The message to be monitored.
	-> PendingAnonymous -> PendingAnonymous
insert time (PendingAnonymousMessage ttl sender text attachments mtime) (PendingAnonymous pa)
	= PendingAnonymous $ Map.insert (text, mtime) (time, ttl, sender, attachments) pa

-- | Returns all anonymous messages and removes them from the structure 
--   that were not posted and that are older than the given time.
purgeNotPosted :: UTCTime -- ^ Timestamp of now.
	-> NominalDiffTime -- ^ Timeout for messages. Messages that were 'insert'ed before this timeout will be returned.
	-> PendingAnonymous
	-> (PendingAnonymous, [ PendingAnonymousMessage ])
purgeNotPosted now timeout (PendingAnonymous pa) = (PendingAnonymous keep, purged')
	where
		(keep, purged) = Map.partitionWithKey (\(_, _) (time, _, _, _) -> (time >= ((-timeout) `addUTCTime` now))) pa
		purged'        = map (\((text, mtime), (_, ttl, sender, attachments)) -> (PendingAnonymousMessage ttl sender text attachments mtime)) $ Map.assocs purged

-- | Purges all messages from the structure whose TTL elapsed.
purgeZeroTTL :: PendingAnonymous -> PendingAnonymous
purgeZeroTTL (PendingAnonymous pa) = PendingAnonymous $ Map.filter (\(_, ttl, _, _) -> ttl > 0) pa

-- | Takes the identifiers of an anonymous message and removes it from the structure (if existing).
posted :: String -- ^ Text of the message.
	-> UTCTime -- ^ Timestamp of the message.
	-> PendingAnonymous -> PendingAnonymous
-- We do this complex check here since local 'UTCTime's contain picoseconds that we do not have from the network.
posted text time (PendingAnonymous pa) = PendingAnonymous $ Map.filterWithKey (\(ltext, ltime) _ -> let dt = (diffUTCTime time ltime) in not (dt < -1 || dt < 1)) pa

