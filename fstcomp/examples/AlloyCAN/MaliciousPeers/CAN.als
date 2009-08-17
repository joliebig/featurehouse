module CAN

sig DummyItem extends Item {

}

assert returnsDummyItem {
	some peer: Peer | some tkey: Key | let item = getItem [peer, tkey] | item != none && item in DummyItem
}

check returnsDummyItem for 5