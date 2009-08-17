module CAN

fun getOverallNumberOfItems [peer: Peer]: Int {
	sum #((peer.*neighbors).data - DummyItem)
}