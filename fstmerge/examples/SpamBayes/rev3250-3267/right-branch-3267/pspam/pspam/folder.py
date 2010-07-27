import ZODB
from Persistence import Persistent
from BTrees.OOBTree import OOBTree, OOSet, difference
import email
import mailbox
import os
import stat
from pspam.message import PMessage
def factory(fp):
    try:
        return email.message_from_file(fp, PMessage)
    except email.Errors.MessageError as msg:
        print(msg)
        return PMessage()
class Folder(Persistent):
    def __init__(self, path):
        self.path = path
        self.mtime = 0
        self.size = 0
        self.messages = OOBTree()
    def _stat(self):
        t = os.stat(self.path)
        self.mtime = t[stat.ST_MTIME]
        self.size = t[stat.ST_SIZE]
    def changed(self):
        t = os.stat(self.path)
        if (t[stat.ST_MTIME] != self.mtime
            or t[stat.ST_SIZE] != self.size):
            return True
        else:
            return False
    def read(self):
        """Return messages added and removed from folder.
        Two sets of message objects are returned.  The first set is
        messages that were added to the folder since the last read.
        The second set is the messages that were removed from the
        folder since the last read.
        The code assumes messages are added and removed but not edited.
        """
        mbox = mailbox.UnixMailbox(open(self.path, "rb"), factory)
        self._stat()
        cur = OOSet()
        new = OOSet()
        while 1:
            msg = next(mbox)
            if msg is None:
                break
            msgid = msg["message-id"]
            cur.insert(msgid)
            if msgid not in self.messages:
                self.messages[msgid] = msg
                new.insert(msg)
        removed = difference(self.messages, cur)
        for msgid in list(removed.keys()):
            del self.messages[msgid]
        return new, OOSet(list(removed.values()))
if __name__ == "__main__":
    f = Folder("/home/jeremy/Mail/INBOX")
