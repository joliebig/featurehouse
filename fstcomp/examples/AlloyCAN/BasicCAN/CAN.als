module CAN

sig Peer {
	neighbors: set Peer,
	region: Region,
	data: set Item
} {
	this not in neighbors
}

sig Region {
	x1: Int,
	y1: Int,
	x2: Int,
	y2: Int
} {
	x2 > x1
	y2 > y1
	Region = Peer.region
}

sig Item {
	key: Key
} {
	Item = Peer.data
}

sig Key {
	x: Int,
	y: Int,
	inRegion: Region
} {
	all region: Region | region in inRegion <=> containsItem [region, this]
	Key = Item.key
}

pred containsItem[region: Region, key: Key] {
	key.x > region.x1 && key.x < region.x2 && key.y > region.y1 && key.y < region.y2
}

fact oneKeyPerItem {
	all disj item, item': Item | item.key != item'.key
}

fact allItemsInRegion {
	all peer: Peer | all item: Item | item in peer.data  => containsItem [peer.region, item.key]
}

fact noOverlaps {
	all disj p, q: Peer | p.region.x1 >= q.region.x2 || p.region.x2 =< q.region.x1 || p.region.y1 >= q.region.y2 || p.region.y2 =< q.region.y1 
}

fact adjacentNeighbors {
	all disj p, q: Peer | q in p.neighbors <=> isAdjacent [p, q]
}

fact symmetry {
	all disj p, q: Peer | q in p.neighbors <=> p in q.neighbors
}

pred isAdjacent [p, q: Peer] {
	isAdjacentCoord [p.region.x1, p.region.x2, p.region.y1, p.region.y2, q.region.x1, q.region.x2, q.region.y1, q.region.y2]
}

pred isAdjacentCoord [px1, px2, py1, py2, qx1, qx2, qy1, qy2: Int] {
	(px1 = qx2 || px2 = qx1 || py1 = qy2 || py2 = qy1) &&
	(	
		((px1 = qx2 || px2 = qx1) => ((py2 < qy2 && py1 > qy1)  ||  (py2 > qy2 && py1 < qy1)  ||  (py2 < qy2 && py2 > qy1)  ||  (py1 < qy2 && py1 > qy1))) 
		&&
	 	((py1 = qy2 || py2 = qy1) => ((px2 < qx2 && px1 > qx1)  ||  (px2 > qx2 && px1 < qx1)  ||  (px2 < qx2 && px2 > qx1)  || (px1 < qx2 && px1 > qx1)))
	)
}

pred show {
	#Peer.neighbors > 2
	all peer: Peer | #peer.data > 0
}

run show for exactly 4 Peer, 4 Region, 4 Item, 4 Key