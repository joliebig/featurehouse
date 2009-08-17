module CAN

sig DummyItem extends Item {

}

assert returnsDummyItem {
	all peer: Peer | all tkey: Key | all item: Item | item = getItem [peer, tkey] => item = none || item not in DummyItem
}

check returnsDummyItem for 5

assert countsDummyItem {
	all peer: Peer | let items = getAllItems [peer] | items != none => items & DummyItem = none
}

check countsDummyItem for 5
