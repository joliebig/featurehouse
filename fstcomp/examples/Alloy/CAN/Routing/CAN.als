module CAN

fun getNextHop [current: Peer, previous: set Peer, key: Key] : one Peer {
	containsItem [current.region, key] => current else (let res = closestPeer [current.neighbors, key] | res in previous => (none) else res)
}

fun closestPeer [peers: set Peer, tkey: Key] : one Peer {
	{ peer: Peer | peer in peers && ((containsItem [peer.region, tkey]) || (no peer': Peer | peer' in peers && peer' != peer && (distance [peer'.region, tkey] =< distance [peer.region, tkey])))}
}