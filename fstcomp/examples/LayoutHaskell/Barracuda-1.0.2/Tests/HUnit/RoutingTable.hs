module Tests.HUnit.RoutingTable where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import Barracuda.RoutingTable
import Network.AdHoc.UserID
import Network.AdHoc.Routing
import Network.Socket
import Tests.Data
import Test.HUnit

routingTableLookup = TestCase $ do
	let now = date_malcomX
	let rt = hello now saddr_cia [user_wechner] (empty :: SimpleRT)
	assertEqual "routeSingle"
		(Just saddr_cia)
		(routeSingle user_wechner rt)
	assertEqual "routeSingle with ignores"
		Nothing
		(routeSingle user_wechner (rt, Set.singleton saddr_cia))

routingTableDelete = TestCase $ do
	let now = date_jfk
	let rt  = routing now saddr_penisland [(user_eq, 2), (user_spuall, 1)] $ routing now saddr_fsf [(user_eq, 1), (user_spuall, 2)] $ (empty :: SimpleRT)
	assertEqual "deletion of an optimal host"
		(Just saddr_fsf)
		(routeSingle user_spuall $ delete saddr_penisland rt)
	assertEqual "deletion of a suboptimal host"
		(Just saddr_penisland)
		(routeSingle user_eq $ delete saddr_fsf rt)

routingTableUsersMergeNeighbors = TestCase $ do
	let now = date_mlk
	let rt =  routing now saddr_cia [(user_eq, 1)]
		$ routing now saddr_fsf [(user_wechner, 1)]
		$ routing now saddr_penisland [(user_igel, 1)]
		$ (empty :: SimpleRT)
	assertEqual "complete userlist"
		(Set.fromList [user_eq, user_wechner, user_igel])
		(userList rt)
	assertEqual "merge routes for"
		(Map.fromList [(user_eq, 2), (user_wechner, 2)])
		(mergeRoutesFor saddr_penisland Set.empty rt)
	assertEqual "neighbors"
		(Set.fromList [saddr_cia, saddr_fsf, saddr_penisland])
		(neighbors rt)
