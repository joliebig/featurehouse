class Stack:
	items = list()

	def push(self, item):
		self.items.append(item)

	def top(self):
		return self.items[-1]

	def pop(self):
		if len(self.items) > 0:
			return self.items.pop()
		else:
			return None