class Stack:
    items = list()
    serialVersionUID = 42
    
    def push(self, item):
        self.items.append(item)
    
    def pop(self):
        if len(self.items) > 0:
            return self.items.pop()
        else:
            return None