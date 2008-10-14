{-# LANGUAGE FlexibleContexts #-}
-- |
-- Maintainer: Stephan Friedrichs, Henning Guenther
--
-- A data structure storing the routes that some messages have failed to take, so they are not
-- tried again.
module Barracuda.PendingRoute (
	PendingRoute,
	empty,
	insert,
	reinsert,
	routeAndDelete,
	purgeZeroTTL
) where

import Barracuda.RoutingTable hiding (empty)
import Barracuda.TimedCollection
import qualified Data.Heap as Heap
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Maybe
import qualified Data.Set as Set
import Data.Time
import Network.AdHoc.Message
import Network.AdHoc.Routing
import Network.Socket

newtype RoutedInfo sign = RoutedInfo (UTCTime, Routed TargetContent sign, Set.Set SockAddr) deriving (Show)

instance Eq (RoutedInfo sign) where
	a == b = timeOut a == timeOut b

instance Ord (RoutedInfo sign) where
	compare = comparing timeOut

newtype PendingRoute sign = NackHeap (Heap.Heap (RoutedInfo sign)) deriving (Show, Eq, Ord)

timeOut :: RoutedInfo sign -> UTCTime
timeOut (RoutedInfo (recv, Routed ttl _ _ _ _, _)) = (fromInteger $ toInteger ttl) `addUTCTime` recv

instance TimedCollection (PendingRoute sign) where
	deleteBefore now nacks@(NackHeap heap) = if now > minDeadline
		then nacks
		else deleteBefore now $ NackHeap heap'
		where (RoutedInfo (minDeadline, _, _), heap') = Heap.deleteFindMin heap

-- | Creates an empty 'PendingRoute'.
empty :: PendingRoute sign
empty = NackHeap Heap.empty

-- | Inserts a new 'Routed' element into the 'PendingRoute'. If there already are hosts
--   that have to be ignored furtheron, use 'reinsert'.
insert :: UTCTime               -- ^ The current point of time.
	-> Routed TargetContent sign -- ^ The message to be saved.
	-> PendingRoute sign -> PendingRoute sign
insert now rtc nacks = reinsert now rtc Set.empty nacks

-- | Inserts a new 'Routed' element into the 'PendingRoute'. It also saves routes that
--   already have been (unsuccessfully) tried before. Compare to 'insert'.
reinsert :: UTCTime             -- ^ The current point of time.
	-> Routed TargetContent sign -- ^ The message to be saved.
	-> Set.Set SockAddr     -- ^ A set of routers that were not able to route the message.
	-> PendingRoute sign -> PendingRoute sign
reinsert now rtc set (NackHeap heap) = NackHeap $ Heap.insert (RoutedInfo (deadline, rtc, set)) heap
	where	deadline = toEnum (fromEnum (routedTTL rtc)) `addUTCTime` now

-- | Tries to reroute stored messages. The function looks at every stored message and
--   proposes a new route. This new route is added to the set of already attempted hosts.
--   All messages with a new route-proposal are removed from the original 'PendingRoute', so
--   only the \"hopeless cases\" remain within.
routeAndDelete :: RoutingStrategy (r,Set.Set SockAddr) =>
	r           -- ^ The 'RoutingTable' with the routing information.
	-> PendingRoute sign -- ^ The 'PendingRoute' storing nacked messages.
	-> (PendingRoute sign, [(SockAddr,Routed TargetContent sign, Set.Set SockAddr,UTCTime)])
	            -- ^ Tuples of messages where a new route has been found. It contains the
		    --   message itself, a set of attempted routes (including the new one) and
		    --   a new router to be tried. The new 'PendingRoute' does not contain the
		    --   newly routed elements.
routeAndDelete table (NackHeap heap) = let
	routingResults = map (\(RoutedInfo (deadline, msg, failedRoutes)) ->
		(deadline,failedRoutes,route (table,failedRoutes) msg)
		) (Heap.toAscList heap)
	noRoutes = catMaybes $ map (\(deadline,failedRoutes,(_,msg)) ->
		msg >>= \rmsg -> return $ RoutedInfo (deadline,rmsg,failedRoutes)
		) routingResults
	routes = concatMap (\(deadline,failed,(mp,_)) ->
		map (\(addr,msg) -> (addr,msg,failed,deadline)) (Map.toList mp)
		) routingResults
	in (NackHeap $ Heap.fromAscList noRoutes,routes)

purgeZeroTTL :: UTCTime -> PendingRoute sign -> (PendingRoute sign,[Routed TargetContent sign])
purgeZeroTTL now (NackHeap heap) = let
	(nheap,removed) = Heap.removeWhile (\ri@(RoutedInfo (_,msg,_)) -> if timeOut ri >= now
		then Just (msg {routedTTL = 0})
		else Nothing) heap
	in (NackHeap nheap,removed)
