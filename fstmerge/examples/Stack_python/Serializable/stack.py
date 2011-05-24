class Stack:
	items = llist()
   
	def push(self, item):
		self.items.append(item)

	serialVersionUID = 42
	
	def pop(self):
		if len(self.items) > 0:
			return self.items.pop()
		else:
			return None