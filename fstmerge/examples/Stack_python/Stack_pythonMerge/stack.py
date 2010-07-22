class  Stack :
	~~FSTMerge~~ items = llist() ##FSTMerge## items = list() ##FSTMerge## items = xlist()
	   
	def push(self, item):

		self.items.append(item)

		
	def pop(self):

		if len(self.items) > 0:

			return self.items.pop()

		else:

			return None

	def flush(self):

		pass


	serialVersionUID = 42


