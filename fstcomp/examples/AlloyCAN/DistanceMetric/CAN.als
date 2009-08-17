module CAN

open util/integer

fun distance [region: Region, key: Key] : one Int {
	min [manhatten[region.x1, region.y1, key.x, key.y], 
            manhatten[region.x2, region.y1, key.x, key.y], 
            manhatten[region.x1, region.y2, key.x, key.y], 
            manhatten[region.x2, region.y2, key.x, key.y]]
}

fun manhatten [x1, y1, x2, y2: Int] : one Int {
	((abs [x1.sub[x2]]).add[abs[y1.sub[y2]]]).sub[7]
}

fun abs [a: Int]: one Int {
	a < 0 => a.mul[-1] else a
}

fun min [a, b, c, d: Int]: one Int {
	(a =< b && a =< c && a =< d) => a else ((b =< a && b =< c && b =< d) => b else ((c =< a && c =< b && c =< d) => c else d))
}