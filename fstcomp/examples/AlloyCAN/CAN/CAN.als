module  CAN open  util/integer sig  Peer {	neighbors: set Peer , 	region: Region , 	data: set Item , 	getItemRel: Peer -> Peer -> Key -> Item} {	this not in neighbors} sig  Region {	x1: Int , 	y1: Int , 	x2: Int , 	y2: Int} {	x2 > x1 	y2 > y1 	Region = Peer.region} sig  Item {	key: Key} {	Item = Peer.data} sig  Key {	x: Int , 	y: Int , 	inRegion: Region} {	all region: Region | region in inRegion <=> containsItem [region, this] 	Key = Item.key} 
pred containsItem[region: Region, key: Key] {	key.x > region.x1 && key.x < region.x2 && key.y > region.y1 && key.y < region.y2
} 
fact oneKeyPerItem {	all disj item, item': Item | item.key != item'.key
} 
fact allItemsInRegion {	all peer: Peer | all item: Item | item in peer.data => containsItem [peer.region, item.key]
} 
fact noOverlaps {	all disj p, q: Peer | p.region.x1 >= q.region.x2 || p.region.x2 =< q.region.x1 || p.region.y1 >= q.region.y2 || p.region.y2 =< q.region.y1
} 
fact adjacentNeighbors {	all disj p, q: Peer | q in p.neighbors <=> isAdjacent [p, q]
} 
fact symmetry {	all disj p, q: Peer | q in p.neighbors <=> p in q.neighbors
} 
pred isAdjacent [p, q: Peer] {	isAdjacentCoord [p.region.x1, p.region.x2, p.region.y1, p.region.y2, q.region.x1, q.region.x2, q.region.y1, q.region.y2]
} 
pred isAdjacentCoord [px1, px2, py1, py2, qx1, qx2, qy1, qy2: Int] {	(px1 = qx2 || px2 = qx1 || py1 = qy2 || py2 = qy1) &&	(	((px1 = qx2 || px2 = qx1) => ((py2 < qy2 && py1 > qy1) || (py2 > qy2 && py1 < qy1) || (py2 < qy2 && py2 > qy1) || (py1 < qy2 && py1 > qy1)))	&&	((py1 = qy2 || py2 = qy1) => ((px2 < qx2 && px1 > qx1) || (px2 > qx2 && px1 < qx1) || (px2 < qx2 && px2 > qx1) || (px1 < qx2 && px1 > qx1)))	)
} 
pred show {	#Peer.neighbors > 2	all peer: Peer | #peer.data > 0
} 
run show for exactly 4 Peer, 4 Region, 4 Item, 4 Key 
fact size {	all key: Key | key.x < 4 && key.x > -4 && key.y < 4 && key.y > -4	all region: Region | region.x1 < 4 && region.x1 > -4 && region.y1 < 4 && region.y1 > -4 && region.x2 < 4 && region.x2 > -4 && region.y2 < 4 && region.y2 > -4
} 
fun distance [region: Region, key: Key] : one Int {	min [manhatten[region.x1, region.y1, key.x, key.y], manhatten[region.x2, region.y1, key.x, key.y], manhatten[region.x1, region.y2, key.x, key.y], manhatten[region.x2, region.y2, key.x, key.y]]
} 
fun manhatten [x1, y1, x2, y2: Int] : one Int {	((abs [x1.sub[x2]]).add[abs[y1.sub[y2]]]).sub[7]
} 
fun abs [a: Int]: one Int {	a < 0 => a.mul[-1] else a
} 
fun min [a, b, c, d: Int]: one Int {	(a =< b && a =< c && a =< d) => a else ((b =< a && b =< c && b =< d) => b else ((c =< a && c =< b && c =< d) => c else d))
} 
fun getNextHop [current: Peer, previous: set Peer, key: Key] : one Peer {	containsItem [current.region, key] => current else (let res = closestPeer [current.neighbors, key] | res in previous => (none) else res)
} 
fun closestPeer [peers: set Peer, tkey: Key] : one Peer {	{ peer: Peer | peer in peers && ((containsItem [peer.region, tkey]) || (no peer': Peer | peer' in peers && peer' != peer && (distance [peer'.region, tkey] =< distance [peer.region, tkey])))}
} 
fun getItem [peer: Peer, tkey: Key] : one Item {	tkey.(Peer.(peer.(Peer.getItemRel)))
} 
fact defineGetItemRel {	all peer, previous: Peer, tkey: Key, item: Item | peer -> previous -> tkey -> item in Peer.getItemRel <=> (((containsItem [peer.region, tkey]) && (item = {item' : peer.data | tkey = item'.key})) || ((!(containsItem [peer.region, tkey])) && (peer != (getNextHop [peer, previous, tkey])) && (#(getNextHop [peer, previous, tkey]) > 0) && ((getNextHop [peer, previous, tkey]) -> (peer + previous) -> tkey -> item in Peer.getItemRel)))
} sig  DummyItem  extends Item {} 
assert returnsDummyItem {	some peer: Peer | some tkey: Key | let item = getItem [peer, tkey] | item != none && item in DummyItem
} 
check returnsDummyItem for 5
