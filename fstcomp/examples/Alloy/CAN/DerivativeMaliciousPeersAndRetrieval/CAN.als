module CAN

fun getItem [peer: Peer, tkey: Key] : one Item {
	tkey.(Peer.(peer.(Peer.getItemRel - (Peer.getItemRel :> DummyItem))))
}