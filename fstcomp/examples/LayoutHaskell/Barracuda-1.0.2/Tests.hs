module Main where

import Network.GnuTLS (withGnuTLS)
import System.IO
import Test.HUnit (runTestTT, Test (TestList))
import Test.QuickCheck (quickCheck)

import Tests.QuickCheck.Encryption
import Tests.QuickCheck.Parser
import Tests.QuickCheck.X509
import Tests.HUnit.ChannelList
import Tests.HUnit.PendingAck
import Tests.HUnit.RoutingTable

main = withGnuTLS $ do
	hSetBuffering stdout NoBuffering
	hSetBuffering stderr NoBuffering
	putStrLn "Running QuickCheck tests:"
	quickCheck testByteStringEncrypt
	quickCheck testGeneratorParserId
	quickCheck rsaParametersMatch
	quickCheck rsaVerify
	quickCheck rsaEncrypt
	putStrLn "Running HUnit tests:"
	runTestTT $ TestList
		[channelList1
		,channelList2
		,routingTableLookup
		,routingTableDelete
		,routingTableUsersMergeNeighbors
		,pendingAck
		]
