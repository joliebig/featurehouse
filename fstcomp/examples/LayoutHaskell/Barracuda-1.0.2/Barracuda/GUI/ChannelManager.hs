-- |
-- Maintainer: Henning Guenther
--
-- The ChannelManager represents the state of the currently displayed channel.
module Barracuda.GUI.ChannelManager
	(ChannelManager(view)
	,channelManagerNew
	,channelManagerChannel
	,channelManagerError
	,channelManagerUsername
	,channelManagerSelect
	,channelManagerCheckState
	,channelManagerRenderState
	,channelManagerUpdate
	,channelManagerUsers
	,channelManagerPost
	,channelManagerWantsAuth
	,channelManagerNoState
	) where

import Control.Monad (unless)
import Data.IORef
import Data.Map as Map
import Data.Set as Set
import Data.Time.Clock

import Network.AdHoc.Channel
import Network.AdHoc.Message
import Network.AdHoc.UserID

import Graphics.UI.Gtk
import Barracuda.GUI.ChatView

data ChannelManagerState
	= DontKnow
	| ListenTo 
	| LookFromOutside
	| ChannelVanished 
	deriving Eq

data ChannelManager = ChannelManager
	{channels :: Channels
	,username :: UserID
	,state :: Maybe ((ChannelName,ChannelID),ChannelManagerState)
	,view :: ChatView
	,downloadCB :: [Attachment] -> IO ()
	,authCB :: ChannelName -> ChannelID -> UserID -> IO ()
	}

type Channels = Map (ChannelName,ChannelID) (String,Bool,Set UserID,Maybe ChatContent)

-- | Creates a new ChannelManager instance
channelManagerNew :: ([Attachment] -> IO ())
		  -> (ChannelName -> ChannelID -> UserID -> IO ())
		  -> UserID
		  -> IO ChannelManager
channelManagerNew cb cb2 name = do
	cv <- chatViewNew
	return $ ChannelManager
		{channels = Map.empty
		,username = name
		,state = Nothing
		,view = cv
		,downloadCB = cb
		,authCB = cb2
		}

-- | Checks if the displaying widget needs to be changed and does so with the
--   provided callback.
channelManagerCheckState :: ChannelManager -> (Widget -> IO ()) -> IO ChannelManager
channelManagerCheckState cm adder = let
	nst = case state cm of
		Nothing -> Nothing
		Just (chan,_) -> Just $ case Map.lookup chan (channels cm) of
			Nothing -> (chan,ChannelVanished)
			Just (_,_,users,_) -> if Set.member (username cm) users
				then (chan,ListenTo)
				else (chan,LookFromOutside)
	in if state cm == nst
		then return cm
		else (do
			(ncm,wid) <- channelManagerRenderState (cm {state = nst})
			adder wid
			return ncm)

-- | Creates a widget to diplay the current state of the manager
channelManagerRenderState :: ChannelManager -> IO (ChannelManager,Widget)
channelManagerRenderState cm = let
	out = do
		lbl <- labelNew (Just $ "You're not a member of this channel.")
		return (cm,toWidget lbl)
	in case state cm of
		Nothing -> do
			lbl <- labelNew (Just "Barracuda - Error")
			return (cm,toWidget lbl)
		Just ((cname,cid),st) -> case st of
			ListenTo -> do
				ncm <- withChatContent cname cid (chatViewSetChatContent (view cm)) cm
				return (ncm,chatViewGetWidget (view cm))
			LookFromOutside -> out
			ChannelVanished -> do
				lbl <- labelNew (Just $ (show cname) ++ " has been closed or does not exist for another reason.")
				return (cm,toWidget lbl)

{- | Update the channel informations of the manager using the contents of an
 -   'AllChans' message.
 -}
channelManagerUpdate :: Map (ChannelName,ChannelID) (String,Bool,Set UserID)
		     -> ChannelManager
		     -> ChannelManager
channelManagerUpdate mp cm = let
		nmp = Map.map (\(name,priv,usrs) -> (name,priv,usrs,Nothing)) mp
		in cm {channels = Map.differenceWith (\(name,priv,usrs,_) (_,_,_,oconts)
			-> Just (name,priv,usrs,case oconts of
				Nothing -> Nothing
				Just roconts -> if Set.member (username cm) usrs
					then Just roconts
					else Nothing)) nmp (channels cm)
			}

-- | Gets the users that needs to be displayed by the 'ChannelList'
channelManagerUsers :: ChannelManager -> [UserID]
channelManagerUsers cm = case state cm of
	Nothing -> []
	Just (chan,_) -> case Map.lookup chan (channels cm) of
		Nothing -> []
		Just (_,_,usrs,_) -> Set.toList usrs

withChatContent :: ChannelName -> ChannelID -> (ChatContent -> IO ()) -> ChannelManager -> IO ChannelManager
withChatContent cname cid f cm = do
	let newcc = do
		ncc <- chatContentNew (chatViewGetWidget $ view cm)
		ncc `onDownload` (downloadCB cm)
		ncc `onAuth` (authCB cm cname cid)
		return ncc
	(cc,ncm) <- case Map.lookup (cname,cid) (channels cm) of
		Just (_,_,_,Just x) -> return (x,cm)
		Just (title,priv,users,Nothing) -> do
			ncc <- newcc
			return (ncc,cm {channels=Map.insert (cname,cid) (title,priv,users,Just ncc) (channels cm)})
		Nothing -> do
			ncc <- newcc
			let n = ("",False,Set.empty,Just ncc)
			return (ncc,cm {channels=Map.insert (cname,cid) n (channels cm)})
	f cc
	return ncm

-- | Display the message of a user in a specific channel.
channelManagerPost :: ChannelName -> ChannelID -> UTCTime -> Maybe UserID -> String -> [Attachment] -> Bool -> ChannelManager -> IO ChannelManager
channelManagerPost cname cid time from text attach delayed cm = do
	ncm <- withChatContent cname cid (chatContentInsert time from text attach delayed) cm
	chatViewScroll (view ncm)
	return ncm

-- | Posts an error into a channel.
channelManagerError :: ChannelName -> ChannelID -> UTCTime -> String -> ChannelManager -> IO ChannelManager
channelManagerError cname cid time text cm = do
	ncm <- withChatContent cname cid (chatContentError time text) cm
	chatViewScroll (view ncm)
	return ncm

-- | Posts an authorization request from another user into a channel.
channelManagerWantsAuth :: ChannelName -> ChannelID -> UTCTime -> UserID -> ChannelManager -> IO ChannelManager
channelManagerWantsAuth cname cid time from cm = do
	ncm <- withChatContent cname cid (chatContentWantAuth time from) cm
	chatViewScroll (view ncm)
	return ncm

channelManagerChannel :: ChannelManager -> Maybe (ChannelName,ChannelID,Bool)
channelManagerChannel cm = state cm >>= \((cname,cid),st) -> return (cname,cid,st == ListenTo)

channelManagerSelect :: ChannelName -> ChannelID -> ChannelManager -> ChannelManager
channelManagerSelect cname cid cm = cm {state = Just ((cname,cid),DontKnow)}

channelManagerNoState :: ChannelManager -> ChannelManager
channelManagerNoState cm = cm {state = Nothing}

channelManagerUsername :: ChannelManager -> UserID
channelManagerUsername = username
