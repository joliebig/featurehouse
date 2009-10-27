#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import sys

class CountConflictingLines:
	def __init__(self, file):
		self.file = file

	def countLines(self):
		myconnum = 0
		yourconnum = 0
		myconflict = False
		yourconflict = False
		self.file = os.path.abspath(self.file)

		if not os.path.exists(self.file):
			print('ERROR: file '+self.file+' not found!')
		else:
			fd = open(os.path.abspath(self.file), 'r')
			for line in fd:
				if line.strip() == "":
					continue
				if '<<<<<<<' in line:
					myconflict = True
					continue
				if '=======' in line:
					myconflict = False
					yourconflict = True
					continue
				if '>>>>>>>' in line:
					yourconflict = False
					continue
				if myconflict:
					myconnum += 1
					continue
				if yourconflict:
					yourconnum += 1
					continue
		return (myconnum, yourconnum)

##################################################
if __name__ == '__main__':
	cs = CountConflictingLines(sys.argv[1])
	print(cs.countLines())