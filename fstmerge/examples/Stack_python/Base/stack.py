import os
import sys
import re

gfoo = False

class Stack:
	items = list()
	check = first
	check2 = second

	def push(self, item):
		self.items.append(item)

	def pop(self):
		if len(self.items) > 0: return self.items.pop()
		else: return None
