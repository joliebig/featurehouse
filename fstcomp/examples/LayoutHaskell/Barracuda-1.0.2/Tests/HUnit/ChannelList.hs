module Tests.HUnit.ChannelList where

import Data.Monoid
import Data.List (sort)
import Data.Set as Set
import Data.Time
import Test.HUnit
import Tests.Data
import Network.AdHoc.Channel
import Network.AdHoc.UserID
import Barracuda.ChannelList

channelList1 = TestCase $ do
	let now = date_mlk
	let cl  =  update now cname_sep07 channel_sep07 "description" True [user_eq]
		 $ update now cname_sep07 channel_csstuds "description" False [user_eq] (mempty :: ChannelMap)
	let cl2 = join user_wechner cname_sep07 channel_sep07 cl
	assertEqual "join and leave on public channel"
		cl
		(leave user_igel cname_sep07 channel_sep07 $ join user_igel cname_sep07 channel_sep07 cl)
	assertEqual "join and leave on private channel"
		cl
		(leave user_igel cname_sep07 channel_csstuds $ join user_igel cname_sep07 channel_csstuds cl)
	assertEqual "complete users set"
		(Set.singleton user_eq)
		(usersAll cl)
	assertEqual "partial users set from public channel"
		(Set.singleton user_eq)
		(usersChannel cname_sep07 channel_csstuds cl)
	assertEqual "partial users set from private channel"
		(Set.singleton user_eq)
		(usersChannel cname_sep07 channel_sep07 cl)

channelList2 = TestCase $ do
	let now   = date_jfk
	let later = 1 `addUTCTime` now
	let cl    = update now cname_csstuds   channel_csstuds   "d1" True [user_spuall]
		  $ update now cname_barracuda channel_barracuda "d2" False [user_igel]
		  $ mempty :: ChannelMap
	assertEqual "channel list"
		[(cname_barracuda, channel_barracuda, "d2", False), (cname_csstuds, channel_csstuds, "d1", True)]
		(sort $ channels cl)
	assertEqual "announce purge"
		(update later cname_barracuda channel_barracuda "d2" False [user_igel] mempty
			,[(cname_barracuda, channel_barracuda, "d2", Set.singleton user_igel, False)])
		(announcePurge (Set.singleton user_igel) later later cl)
