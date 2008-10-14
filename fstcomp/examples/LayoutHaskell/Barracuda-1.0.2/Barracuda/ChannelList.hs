-- |
-- Maintainer: Stephan Friedrichs
--
-- This module represents the channel logic of the protocoll; i.e. for example when channels have
-- to be announced, merged and deleted or the management of private keys associated with private
-- channels.
module Barracuda.ChannelList (
	ChannelList(..),
	KeyList(..),
	ChannelMap
) where

import Barracuda.TimedCollection
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Set as Set
import Data.Time
import Data.Word
import Network.AdHoc.Channel
import Network.AdHoc.UserID
import Prelude hiding (lookup)

-- | A @ChannelList@ manages information about channelnames, -types and -members retreived
--   via the network. Note that this datastructure only contains information about public
--   and private channels and not about the anonymous channel.
class (Monoid c, TimedCollection c) => ChannelList c where
	-- | Lets the given user join a channel. If the cannel is unknown, the information is ignored.
	--   Normally this function is called to handle the protocols JOIN messages.
	join          :: UserID -> ChannelName -> ChannelID -> c -> c
	-- | Makes a user leave a channel. Called to suffice the protocols LEAVE messages.
	leave         :: UserID -> ChannelName -> ChannelID -> c -> c
	-- | This function takes the information of a channel broadcast and updates the collection.
	--   According to the protocol specs, it merges public channels with the same name and
	--   different ids, but keeps private channels with the same name and different ids
	--   separated.
	update        :: UTCTime -- ^ Timestamp for the channel announcement
		-> ChannelName   -- ^ The Name of the affected channel
		-> ChannelID     -- ^ ID of the affected channel
		-> String        -- ^ The channel description
		-> Bool          -- ^ True if the channel is private
		-> [UserID]      -- ^ The users within the concerned channel
		-> c -> c
	-- | Retrieves all information about a given combination of 'ChannelName'
	--   and 'ChannelID' if it is known.
	lookup        :: ChannelName -> ChannelID -> c -> Maybe (Bool, String, Set.Set UserID)
	-- | Looks up all information about a public channel with the given 'ChannelName', if known.
	lookupPublic  :: ChannelName -> c -> Maybe (ChannelID, String, Set.Set UserID)
	-- | Retreives a list of all documented channels. The @String@ is the channel
	--   description, the @Bool@ indicates whether they are private.
	channels      :: c -> [(ChannelName, ChannelID, String, Bool)]
	-- | Find out whether a channel with the given name exists.
	exists        :: ChannelName -> c -> Bool
	-- | Find out, if a given Channel is private. If and only if the combination
	--   of ('ChannelName', 'ChannelID') is unknown, 'Nothing' is returned. As a
	--   consequence, 'Just' indicates, that the combination is definitely stored.
	isPrivate     :: ChannelName -> ChannelID -> c -> Maybe Bool
	-- | Finds all users in any channel.
	usersAll      :: c -> Set.Set UserID
	-- | Retreives all users in a given channel.
	usersChannel  :: ChannelName -> ChannelID -> c -> Set.Set UserID
	-- | This function takes a set of locally active users, the current time and a new timestamp.
	--   It returns a list of channels, that have to be announced as their timestamps have run out
	--   and a local user is active in them. The new 'ChannelList' returned is purged of all channels
	--   whose timeouts have run out and that don't have a local active user; the timestamps
	--   of the priorily mentioned channels (local user active and timestamp run out) have been updated
	--   as well.
	announcePurge :: Set.Set UserID -- ^ All locally active users
		-> UTCTime              -- ^ The current point of time
		-> UTCTime              -- ^ A new timestamp
		-> c -> (c, [(ChannelName, ChannelID, String, Set.Set UserID, Bool)])
	-- | Returns a map suitable for using in 'Barracuda.GUI.ServerInterface.ControlResponse'.
	channelMap    :: c -> Map.Map (ChannelName, ChannelID) (String, Bool, Set.Set UserID)

-- | A class representing collections able to store the symmetric keys for private channels.
--   Each private channel is represented by its 'ChannelName' and 'ChannelID' and may be
--   associated either with its key or a timestamp of the last key request. The default
--   implementation is 'ChannelMap'.
class Monoid k => KeyList k where
	-- | Log that the key for the given channel has been requested at the given point of time.
	--   This does not override an already retreived key (in this case this call is just ignored).
	request  :: UTCTime -> ChannelName -> ChannelID -> k -> k
	-- | Insert a successfully requested key into the 'KeyList'.
	insert   :: ChannelName -> ChannelID -> Word64 -> k -> k
	-- | Gets the key for a channel, if and only if it is known and the user is allowed to have it.
	getKey   :: UserID -> ChannelName -> ChannelID -> k -> Maybe Word64
	-- | Returns if the key for the given channel has already been locally stored.
	keyKnown :: ChannelName -> ChannelID -> k -> Bool
	-- | Returns a list of unanswered requests above a certain timeout.
	unknown  :: UTCTime         -- ^ The current point of time.
		-> NominalDiffTime  -- ^ An acceptable timeout. Requests within this timeout are not returned.
		-> k -> [(ChannelName, ChannelID)]

-- | The default implementation of 'ChannelList'.
data ChannelMap = ChannelMap
	{ public  :: Map.Map  ChannelName (ChannelID,  String, Set.Set UserID, UTCTime)
	, private :: Map.Map (ChannelName, ChannelID) (String, Set.Set UserID, UTCTime, KeyStatus Word64)
	} deriving (Show, Eq, Ord)

data KeyStatus a
	= NoInterest
	| Requested UTCTime
	| Acquainted a
	deriving (Show, Eq, Ord)

instance Monoid ChannelMap where
	mempty = ChannelMap Map.empty Map.empty
	mappend (ChannelMap m1 m2) (ChannelMap n1 n2) = ChannelMap (Map.union m1 n1) (Map.union m2 n2)

instance TimedCollection ChannelMap where
	deleteBefore now cm = cm
		{ public  = Map.filter (\(_, _, _, time) -> time >= now) $ public  cm
		, private = Map.filter (\(_, _, time, _) -> time >= now) $ private cm
		}

instance ChannelList ChannelMap where
	join uid name cid cm = if name == anonymous then cm else cm
		{ public  = Map.adjust (\(cid', msg, users, time) -> if cid == cid'
			then (cid, msg, Set.insert uid users, time)
			else (cid', msg, users, time)) name $ public cm
		, private = Map.adjust (\(msg, users, time, key) -> (msg, Set.insert uid users, time, key))
			(name, cid) $ private cm
		}
	leave uid name cid cm = if name == anonymous then cm else cm
		{ public  = Map.update (\(cid', msg, users, time) -> if cid' == cid
				then let users' = Set.delete uid users in if Set.null users'
					then Nothing
					else Just (cid, msg, users', time)
				else Just (cid', msg, users, time)
			) name $ public cm
		, private = Map.update (\(msg, users, time, key) -> let users' = Set.delete uid users in if Set.null users'
				then Nothing
				else Just (msg, users', time, key)
			) (name, cid) $ private cm
		}
	update time name cid msg encrypted users cm = if name == anonymous then cm else if encrypted
		then cm { private = Map.insertWith
				(\(msg, users, time, _) (_, _, _, key) -> (msg, users, time, key))
				(name, cid) (msg, Set.fromList users, time, NoInterest) $ private cm
			}
		else cm { public  = Map.insertWith
				(\(cid, msg, users, time) (cid', _, users', _) -> (
					min cid cid', msg,
					if cid == cid' then users else Set.union users users',
					time)
				)
				name (cid, msg, Set.fromList users, time) $ public cm
			}
	lookup name cid cm = case Map.lookup name (public cm) of
			Just (cid1, msg1, users1, _) -> if cid == cid1
				then Just (False, msg1, users1)
				else privateResult
			Nothing                      -> privateResult
		where privateResult = do
			(msg2, users2, _, _) <- Map.lookup (name, cid) (private cm)
			return (True, msg2, users2)
	lookupPublic name cm = Map.lookup name (public cm) >>= return.(\(cid, msg, users, _) -> (cid, msg, users))
	channels cm = (Map.foldWithKey (\ n     (i, m, _, _) cs -> (n, i, m, False):cs) [] $ public  cm)
		   ++ (Map.foldWithKey (\(n, i) (m, _, _, _) cs -> (n, i, m, True ):cs) [] $ private cm)
	exists name cm = Map.member name (public cm) || Set.member name (Set.map fst (Map.keysSet (private cm)))
	isPrivate name cid cm = lookup name cid cm >>= return.(\(p, _, _) -> p)
	usersAll cm = Set.union
		(Map.fold (\(_, _, u, _) users -> Set.union users u) Set.empty $ public  cm)
		(Map.fold (\(_, u, _, _) users -> Set.union users u) Set.empty $ private cm)
	usersChannel name cid cm = case Map.lookup name (public cm) of
		Just (cid', _, users, _) -> if cid == cid' then users else privateUsers
		Nothing                  -> privateUsers
		where privateUsers = case Map.lookup (name, cid) (private cm) of
			Nothing               -> Set.empty
			Just (_, users, _, _) -> users
	announcePurge users now ts cm = let
			(ann1, public') = Map.mapAccumWithKey
				(\ann name (cid, msg, users', time) -> if (time < now)
					&& not (Set.null $ Set.intersection users' users)
					then ((name, cid, msg, users', False):ann, (cid, msg, users', ts))
					else (ann, (cid, msg, users', time))
				) [] $ public cm
			(ann2, private') = Map.mapAccumWithKey
				(\ann (name, cid) (msg, users', time, key) -> if (time < now)
					&& not (Set.null $ Set.intersection users' users)
					then ((name, cid, msg, users', True):ann, (msg, users', ts, key))
					else (ann, (msg, users', time, key))
				) [] $ private cm
		in (deleteBefore now cm { public = public', private = private' }, ann1 ++ ann2)
	channelMap cm = let
		p1 = map (\(name, (cid, title, users, _)) -> ((name, cid),(title, False, users))) (Map.toList (public cm))
		p2 = map (\(key, (title, users, _, _))    -> (key, (title, True, users))) (Map.toList (private cm))
		in Map.fromList $ p1 ++ p2

instance KeyList ChannelMap where
	request now name cid cm = cm
		{ private = Map.adjust (\(msg, users, time, key) -> (msg, users, time, max key (Requested now)))
			(name, cid) $ private cm
		}
	insert name cid key cm = cm
		{ private = Map.adjust (\(msg, users, time, _) -> (msg, users, time, Acquainted key))
			(name, cid) $ private cm
		}
	getKey uid name cid cm = case Map.lookup (name, cid) (private cm) of
		Just (_, users, _, Acquainted key) -> if Set.member uid users then Just key else Nothing
		_                                  -> Nothing
	keyKnown name cid cm = case Map.lookup (name, cid) (private cm) of
		Just (_, _, _, Acquainted _) -> True
		_                            -> False
	unknown now timeout cm = Map.foldWithKey (\(name, cid) (_, _, _, key) requests -> case key of
			Requested time -> if now > addUTCTime timeout time
				then (name, cid) : requests
				else requests
			_              -> requests
		) [] $ private cm

