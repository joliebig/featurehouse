module CAN

fact singlePartition {
	all disj peer, peer': Peer | (peer' in peer.^neighbors) && (peer in peer'.^neighbors)
}
