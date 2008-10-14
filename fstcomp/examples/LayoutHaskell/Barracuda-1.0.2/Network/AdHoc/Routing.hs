{-# LANGUAGE FlexibleContexts #-}
-- |
-- Maintainer: Stephan Friedrichs, Henning Guenther
module Network.AdHoc.Routing (
	RoutingStrategy(..),
	Addressed(..)
) where

import Network.AdHoc.UserID

import Data.List as List
import Data.Map as Map
import Data.Word
import Network.Socket
import Network.AdHoc.Message
import Network.AdHoc.Encryption

-- | This class abstracts routing strategies for data-structures like 'Barracuda.RoutingTable.SimpleRT'.
--   Minimal definition: one of 'routeSingle' or 'routeMulti'.
class RoutingStrategy rs where
	-- | Tries to find a route for a given user. The 'SockAddr' is the starting
	--   point of the route.
	routeSingle :: UserID -> rs -> Maybe SockAddr
	routeSingle uid strat = let mp = fst (routeMulti [uid] strat) in case Map.keys mp of
		k:_ -> Just k
		_   -> Nothing
	-- | Find routes for many users. The first object in the tuple maps nodes to a
	--   list of users that should be reached over it. The second one is a list of
	--   users that couldn't be reached.
	routeMulti  :: [UserID] -> rs -> (Map SockAddr [UserID],[UserID])
	routeMulti users strat = foldl (\(mp,nor) user -> case routeSingle user strat of
		Nothing   -> (mp,user:nor)
		Just addr -> (Map.alter ((Just).maybe [user] (user:)) addr mp,nor)) (Map.empty, []) users

-- | Abstracts addressed contents that can be sent to (several) users.
class Addressed a where
	-- | Routes an 'Addressed' datagram.
	route :: RoutingStrategy r =>
		r                            -- ^ The 'RoutingStrategy' to locate users.
		-> a                         -- ^ The data to be routed.
		-> (Map SockAddr a, Maybe a) -- ^ Starting-points and target-data of routes and,
		                             --   optionally, an addressed, failed-to-route datagram.

instance Addressed (Routed (RSAEncrypted String) sign) where
	route strategy rt = routingSingleMap (routedUserID rt) rt strategy
instance Addressed (Routed TargetContent sign) where
	route s rt@(Routed _ _ _ cont _) = case cont of
		Nack srt		-> routingSingleMap (routedUserID srt) rt s
		GetCertificate for	-> routingSingleMap for rt s
		Certificate recv _ _	-> routingMultiMap recv (\nrecv -> rt
			{routedContent = cont {certificateReceivers = nrecv}}) s
		Message recv _ _ _ _ _	-> routingMultiMap recv (\nrecv -> rt
			{routedContent = cont {messageReceivers = nrecv}}) s
		GetKey recv _ _		-> routingSingleMap recv rt s
		Key recv _ _ _ _	-> routingSingleMap recv rt s

routingSingleMap :: RoutingStrategy rs => UserID -> Routed t sign -> rs -> (Map SockAddr (Routed t sign),Maybe (Routed t sign))
routingSingleMap user obj = (maybe (Map.empty,Just obj) (\raddr -> (Map.singleton raddr obj,Nothing))).(routeSingle user)

routingMultiMap :: RoutingStrategy rs => [UserID] -> ([UserID] -> Routed t sign) -> rs -> (Map SockAddr (Routed t sign),Maybe (Routed t sign))
routingMultiMap users f s = let
	(succ,nor) = routeMulti users s
	in (Map.map f succ,case nor of
		[] -> Nothing
		_  -> Just (f nor))

instance Ord SockAddr where
	compare (SockAddrUnix a)     (SockAddrUnix b)     = compare a b
	compare (SockAddrUnix _)     (SockAddrInet _ _)   = LT
	compare (SockAddrInet _ _)   (SockAddrUnix _)     = GT
	compare (SockAddrInet p1 h1) (SockAddrInet p2 h2) = case compare h1 h2 of
		EQ -> compare p1 p2
		x  -> x
