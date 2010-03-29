module CAN

sig Peer {
	getItemRel: Peer -> Peer -> Key -> Item
}

fun getItem [peer: Peer, tkey: Key] : one Item {
	tkey.(Peer.(peer.(Peer.getItemRel)))
}

fact defineGetItemRel {
	all peer, previous: Peer, tkey: Key, item: Item | peer -> previous -> tkey -> item in Peer.getItemRel <=> (((containsItem [peer.region, tkey])  && (item = {item' : peer.data | tkey = item'.key})) || ((!(containsItem [peer.region, tkey]))  && (peer != (getNextHop [peer, previous, tkey]))  && (#(getNextHop [peer, previous,  tkey]) > 0) && ((getNextHop [peer, previous,  tkey]) -> (peer + previous) -> tkey -> item in Peer.getItemRel)))
}