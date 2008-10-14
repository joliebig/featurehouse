-- |
-- Maintainer: Henning Guenther
--
-- A datastructure storing messages for that we await an ACK.
module Barracuda.PendingAck
	(PendingAck()
	,empty
	,insert
	,reinsert
	,purgeNoAck
	,ack
	) where

import Data.Time.Clock
import Data.List as List hiding (insert)
import Data.Map as Map hiding (empty,insert)
import Data.Set as Set hiding (empty,insert)
import qualified Data.Map as Map (empty,insert)
import qualified Data.Set as Set (empty,insert)
import Network.AdHoc.UserID
import Network.AdHoc.Message
import Network.AdHoc.MessageID
import Network.AdHoc.Routing
import Network.Socket(SockAddr)

newtype PendingAck sign = PendingAck (Map (UserID,MessageID,SockAddr) (TTL,TargetContent,sign,UTCTime,UTCTime,Set SockAddr))
	deriving (Show,Eq)

-- | Insert a new message with the time it was received into the 'PendingAck' data structure.
insert :: UTCTime			-- ^ The time the message was sent
       -> Routed TargetContent sign	-- ^ The message
       -> SockAddr			-- ^ The address the message was sent to
       -> PendingAck sign		-- ^ The 'PendingAck'-structure in which to insert
       -> PendingAck sign
insert sent_time rt addr = reinsert sent_time sent_time rt addr Set.empty

-- | Insert a message that already was unsuccessfully routed over a number of hosts.
reinsert :: UTCTime			-- ^ The time the message was sent for the first time
         -> UTCTime			-- ^ The time when the message was sent last
	 -> Routed TargetContent sign	-- ^ The message
	 -> SockAddr			-- ^ The address the message was sent to
	 -> Set SockAddr		-- ^ Hosts that were unsuccessfully used in the routing process
	 -> PendingAck sign		-- ^ The 'PendingAck'-structure in which to insert
	 -> PendingAck sign
reinsert sent_time ack_time (Routed ttl user msgid tc sig) addr tried (PendingAck mp)
	= PendingAck $ Map.insert (user,msgid,addr) (ttl,tc,sig,sent_time,ack_time,tried) mp

-- | Creates an empty 'PendingAck'-structure
empty :: PendingAck sign
empty = PendingAck Map.empty

-- | Removes and returns all messages whose TTL has run out or received no ack
purgeNoAck :: UTCTime		-- ^ Now
	   -> NominalDiffTime	-- ^ How long to wait for an ACK-message
	   -> PendingAck sign
	   -> (PendingAck sign,[(Routed TargetContent sign,UTCTime,Set SockAddr)])
purgeNoAck time ack_diff (PendingAck mp) = let
	(purge,keep) = Map.partition
		(\(ttl,_,_,recv_time,ack_time,_) -> ((-ack_diff) `addUTCTime` time) > ack_time
			|| floor (diffUTCTime time recv_time) > ttl)
		mp
	purgeList = List.map
		(\((receiver,msgid,sent_to),(ttl,cont,sig,recv_time,_,tried)) -> (Routed ttl receiver msgid cont sig,recv_time,Set.insert sent_to tried))
		(Map.toList purge)
	in (PendingAck keep,purgeList)

-- | To be called when an ACK-message was received
ack :: UserID		-- ^ User of the received ACK
    -> MessageID	-- ^ Message-id of the received ACK
    -> SockAddr		-- ^ The address that sent the ACK
    -> PendingAck sign
    -> PendingAck sign
ack user msg addr (PendingAck mp) = PendingAck $ Map.delete (user,msg,addr) mp
