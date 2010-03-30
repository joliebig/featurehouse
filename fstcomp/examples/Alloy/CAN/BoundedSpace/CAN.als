module CAN

fact size {
	all key: Key | key.x < 4 && key.x > -4 && key.y < 4 && key.y > -4
	all region: Region | region.x1 < 4 && region.x1 > -4 && region.y1 < 4 && region.y1 > -4 && region.x2 < 4 && region.x2 > -4 && region.y2 < 4 && region.y2 > -4
}