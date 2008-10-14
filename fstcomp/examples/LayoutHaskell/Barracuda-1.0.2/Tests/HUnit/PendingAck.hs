module Tests.HUnit.PendingAck where

import qualified Data.Set as Set
import Data.Time
import Barracuda.PendingAck

import Tests.Data
import Test.HUnit

pendingAck = TestCase $ do
	let now     = date_malcomX
	let later9  = addUTCTime  9 now
	let later10 = addUTCTime 10 now
	let later11 = addUTCTime 11 now
	let pa1     = insert later10 routed_nack saddr_cia
	            $ insert later9  routed_nottl saddr_penisland -- This message gets removed, because it has no more time to live
		    $ insert now routed_innocent saddr_cia 
		    $ empty
	assertEqual "Purge two messages"
		[(routed_nottl,later9,Set.empty),(routed_innocent,now,Set.empty)]
		(snd $ purgeNoAck later11 3 pa1)
	assertEqual "Ack one message and a few invalids"
		( insert later10 routed_nack  saddr_cia
		$ insert later9  routed_nottl saddr_penisland 
		$ empty)
		( ack user_igel    "1788" saddr_fsf	-- Should be ignored, wrong host
		$ ack user_wechner "29"   saddr_cia
		$ ack user_spuall  "278"  saddr_cia		-- Should be ignored, wrong message-id
		$ pa1)
