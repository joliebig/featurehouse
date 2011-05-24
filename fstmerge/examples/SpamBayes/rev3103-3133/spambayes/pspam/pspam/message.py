import ZODB

from Persistence import Persistent

from email.Message import Message

class  PMessage (Message, Persistent) :
	def __hash__(self):

        return id(self)



