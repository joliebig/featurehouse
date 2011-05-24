module CAN

assert isConnected {
	all peer: Peer | (peer.*neighbors).data = Item
}

check isConnected for 4

fun getOverallNumberOfItems [peer: Peer]: Int {
	#(getAllItems [peer])
}

fun getAllItems [peer: Peer]: set Item {
	(peer.*neighbors).data
}

fun getOverallNumberOfPeers [peer: Peer]: Int {
	#(getAllPeers [peer])
}

fun getAllPeers [peer: Peer]: set Peer {
	peer.*neighbors
}

fun getAverageLoad [peer: Peer]: Int {
	let res = getOverallNumberOfItems [peer].div [(getOverallNumberOfPeers [peer])] | res < 1 => 1 else res
}

pred higherLoad [peer: Peer] {
	#(peer.data) > getAverageLoad [peer]
}