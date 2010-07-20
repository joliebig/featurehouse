class Stack:
    items = list()
    
    def push(self, item):
        self.items.append(item)
    
    def size(self):
        return len(self.items)
    
    def pop(self):
        if len(self.items) > 0:
            return self.items.pop()
        else:
            return None