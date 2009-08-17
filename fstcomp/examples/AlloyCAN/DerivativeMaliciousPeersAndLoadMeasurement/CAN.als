module CAN

fun getAllItems [peer: Peer]: set Item {
	(peer.*neighbors).data - DummyItem
}