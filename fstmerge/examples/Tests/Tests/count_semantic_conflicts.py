#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import sys

class CountSemConflicts:
	def __init__(self, file):
		self.file = file

	def countSemConflicts(self):
		conflicts = 0
		self.file = os.path.abspath(self.file)

		if not os.path.exists(self.file):
			print('ERROR: file '+self.file+' not found!')
		else:
			fd = open(os.path.abspath(self.file), 'r')
			for line in fd:
				conflicts += line.count('~')

		return conflicts

##################################################
if __name__ == '__main__':
	cs = CountSemConflicts(sys.argv[1])
	print(cs.countSemConflicts())
