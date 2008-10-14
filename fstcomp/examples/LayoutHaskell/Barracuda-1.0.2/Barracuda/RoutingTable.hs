{-# LANGUAGE FlexibleContexts #-}
-- |
-- Maintainer: Stephan Friedrichs
-- 
-- This module stores the entire routing logic of our implementation.
module Barracuda.RoutingTable (
	-- * Routing
	-- ** Definition
	RoutingStrategy(..), RoutingTable(..),
	-- ** Implementation
	SimpleRT,
	-- * Configuration
	hopMax
) where

import Barracuda.TimedCollection
import Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Time
import Network.AdHoc.Routing -- as Routing
import Network.AdHoc.UserID
import Network.Socket

-- | A routing table is an efficient data structure for the handling of routing
--   information. It must maintain a list of hosts together with the
--   information which users it knows and how many hops it takes to reach them.
class (TimedCollection r, RoutingStrategy r, RoutingStrategy (r, Set.Set SockAddr)) =>
		RoutingTable r where
	-- | Construct an empty routing table.
	empty               :: r
	-- | Updates the routing table with information about which users
	--   reside on a given host. Usually obtained through a HELLO protocol-message.
	hello               :: UTCTime -> SockAddr -> [UserID] -> r -> r
	-- | Given a timestamp, a 'SockAddr' and the routing table of a host,
	--   this function updates the available routing information. This
	--   information is usually provided by a ROUTING protocol-message,
	--   therefore each hop count is increased by 1.
	routing             :: UTCTime -> SockAddr -> [(UserID, Int)] -> r -> r
	-- | Deletes a given host and all related routing information from the
	--   routing table. Usually this is triggered by that host leaving the
	--   transmission range.
	delete              :: SockAddr -> r -> r
	-- | Gives a set of all users that are known through the information
	--   in the routing table.
	userList            :: r -> Set.Set UserID
	-- | Merges all known routes to one map. The 'Set.Set' of 'UserID' are the local
	--   users that are deleted from the data.
	mergeRoutes         :: Set.Set UserID -> r -> Map.Map UserID Int
	-- | Put together routing informations that have to be send to a given
	--   host, wich includes deleting the routing information from that host.
	--   The 'Set.Set' of 'UserID' are the local users that are removed from the
	--   returned 'Map.Map'.
	--   This function does not have to be implemented, there is a default implementation.
	mergeRoutesFor      :: SockAddr -> Set.Set UserID -> r -> Map.Map UserID Int
	mergeRoutesFor addr users = (mergeRoutes users).(delete addr)
	-- | Get a set of direct neighbors (nodes that are reachable without routing).
	neighbors           :: r -> Set.Set SockAddr

-- | The default implementation of 'RoutingTable'.
newtype SimpleRT = SimpleRT (Map.Map SockAddr (UTCTime, Set.Set UserID, Map.Map UserID Int)) deriving Show

instance TimedCollection SimpleRT where
	deleteBefore time (SimpleRT mp)   = SimpleRT $ Map.filter ((>=time).(\(x, _, _) -> x)) mp

instance RoutingStrategy SimpleRT where
	routeSingle user tb = routeSingle user (tb, Set.empty :: Set.Set SockAddr)

instance RoutingStrategy (SimpleRT, Set.Set SockAddr) where
	routeSingle user (SimpleRT mp, hosts)
		| null routes             = Nothing
		| otherwise               = Just (fst $ minRoute routes)
		where
		allPeers       = Map.assocs mp
		routes         = directRoutes ++ indirectRoutes
		directRoutes   = mapMaybe (\(k, (_, set, _)) -> if Set.member user set && Set.notMember k hosts
				then Just (k, 1)
				else Nothing
			) allPeers
		indirectRoutes = mapMaybe (\(k, (_, _, table)) -> do
				hops <- Map.lookup user table
				if Set.member k hosts then fail "ignored host" else return (k, hops)
			) allPeers

-- | Find the shortest route. The only difference to @minimumBy (comparing snd)@ is, that this
--   function immediately returns a route with 1 hop. This function is undefined for an empty list.
minRoute :: [(a, Int)] -> (a, Int)
minRoute []             = error "Barracuda.RoutingTable.minRoute: empty list"
minRoute [tuple]        = tuple
minRoute ((x, hops):xs) = if hops <= 1
	then (x, hops)
	else let (x', hops') = minRoute xs in if hops < hops'
		then (x, hops)
		else (x', hops')

instance RoutingTable SimpleRT where
	empty                              = SimpleRT Map.empty
	hello time addr xs (SimpleRT mp)   = SimpleRT $ Map.insertWith (\(t1, h, _) (t2, _, r) -> (max t1 t2, h, r))
		addr (time, Set.fromList xs, Map.empty) mp
	routing time addr xs (SimpleRT mp) = SimpleRT $ Map.insertWith (\(t1, _, r) (t2, h, _) -> (max t1 t2, h, r))
		addr (time, Set.empty, routingMessage) mp
		where routingMessage = Map.fromList $ filter (\(_, h) -> h <= hopMax) $ map (\(u, h) -> (u, h + 1)) xs
	delete addr (SimpleRT mp)          = SimpleRT $ Map.delete addr mp
	userList (SimpleRT mp)             = Set.unions [ helloUsers | (_, helloUsers, _) <- values ]
		`Set.union` Set.unions [ Map.keysSet routingUsers | (_, _, routingUsers) <- values ]
		where values = Map.elems mp
	mergeRoutes users (SimpleRT mp)    = (flip (Foldable.foldl (\m user -> Map.insert user 0 m))) users -- add local users
		$ Map.unionsWith min                       -- always prefer shorter over long routes: length does not count :P
		$ fmap (\(_, h, r) -> Foldable.foldl (\mp u -> Map.insert u 1 mp) r h) -- prefer users from hello over indirect
		$ Map.elems mp                                                         -- routing info
	neighbors (SimpleRT mp)            = Map.keysSet mp

instance RoutingStrategy (SimpleRT, Set.Set UserID) where
	routeSingle user (rt, _)           = routeSingle user rt
	routeMulti users (rt, locals)      = routeMulti (List.filter (\x -> Set.member x locals) users) rt

-- | The upper hop-limit for routig entries. It is used to detect dead
--   entries in routing messages.
hopMax :: Num n => n
hopMax = 360

