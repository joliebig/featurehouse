import os
import time
import gtk
import gobject
import emesenelib.common
import desktop
class FileTransfer:
    ''' this class represents one filetransfer'''
    def __init__(self, controller, p2p, conversation, session, context, sender):
        '''Constructor
        The sender parameter accepts the special value "Me", which is not
        a valid mail, and is used for sent files'''
        self.WAITING = 0
        self.TRANSFERING = 1
        self.RECEIVED = 2
        self.FAILED = 3
        self.controller = controller
        self.p2p = p2p
        self.conversation = conversation
        self.session = int(session)
        self.context = context
        self.sender = sender
        self.receivedBytes = 0
        self.state = self.WAITING
        self.timeAccepted = None
        self.previewImage = None
        if context.preview != '':
            try:
                loader = gtk.gdk.PixbufLoader()
                loader.write(context.preview)
                loader.close()
                self.previewImage = loader.get_pixbuf()
                del loader
            except gobject.GError:
                del loader
        self.localPath = None
        if sender == 'Me':
            self.conversation.appendOutputText(None, \
                _('Sending %s') % self.context.filename, 'information')
        else:
            args = {'mail': self.p2p.mail, 'file': self.context.filename}
            self.conversation.appendOutputText(None, \
                _('%(mail)s is sending you %(file)s') % args, \
                'information')
        self.signals = []
        sap = self.signals.append
        sap(self.p2p.connect('transfer-progress', self.onFtProgress))
        sap(self.p2p.connect('transfer-failed', self.onFtFailed))
        sap(self.p2p.connect('file-transfer-complete', self.onFtReceived))
        if sender == 'Me':
            sap(self.p2p.connect('file-transfer-accepted', self.onFtAccepted))
    def disconnect(self):
        for identifier in self.signals:
            self.p2p.disconnect(identifier)
    def getPreviewImage(self):
        return self.previewImage
    def getBytes(self):
        '''returns a tuple with received and total bytes'''
        return int(self.receivedBytes), int(self.context.file_size)
    def accept(self):
        '''accept this transfer'''
        self.conversation.appendOutputText(None, \
            _('You have accepted %s') % self.context.filename, \
            'information')
        self.p2p.emit('file-transfer-accepted', self.session, self.context,
            self.sender)
        self.timeAccepted = time.time()
        self.state = self.TRANSFERING
        self.ui.stateChanged()
    def cancel(self):
        '''cancel this transfer'''
        self.conversation.appendOutputText(None,
            _('You have canceled the transfer of %s') % self.context.filename,
            'information')
        self.p2p.emit('file-transfer-canceled', self.session, self.context,
            self.sender)
        self.state = self.FAILED
        self.ui.stateChanged()
        self.remove()
    def getElapsedTime(self):
        '''return the time elapsed since this transfer was accepted'''
        if not self.timeAccepted:
            return 0
        else:
            return int(time.time()) - int(self.timeAccepted)
    def getAverageSpeed(self):
        '''get average file transfer speed (bytes per second)'''
        time = self.getElapsedTime()
        if time == 0: # prevent division by zero
            return 0
        else: 
            return self.receivedBytes / time
    def remove(self):
        '''remove this transfer'''
        self.conversation.transfers.remove(self)
        self.disconnect()
    def onFtAccepted(self, p2p, session, context, sender):
        if session != self.session: return
        self.conversation.appendOutputText(None, \
            _('Starting transfer of %s') % self.context.filename, \
            'information')
        self.timeAccepted = time.time()
        self.state = self.TRANSFERING
        self.ui.stateChanged()
    def onFtProgress(self, switchboard, session, bytes):
        if session != self.session: return
        self.receivedBytes = int(bytes)
        self.ui.updateProgress()
    def onFtFailed(self, switchboard, session, reason):
        if session != self.session: return
        output = _('Transfer of %s failed.') % self.context.filename + ' ' 
        if reason == 'corrupt':
            output += _('Some parts of the file are missing')
        elif reason == 'cancelled':
            output += _('Interrupted by user')
        elif reason == 'error':
            output += _('Transfer error')
        self.conversation.appendOutputText(None, output, 'error')
        self.state = self.FAILED
        self.ui.stateChanged()
    def onFtReceived(self, p2p, session, context, src, sender):
        '''called when filetransfer is finished'''
        if session != self.session: return
        if sender == 'Me':
            self.conversation.appendOutputText(None, \
                _('%s sent successfully') % context.filename, 'information')
        else:
            self.conversation.appendOutputText(None, \
                _('%s received successfully.') % context.filename, \
                'information')
        self.conversation.doMessageWaiting(sender)
        self.receivedBytes = context.file_size
        self.state = self.RECEIVED
        self.ui.stateChanged()
        if sender == 'Me' or src.closed:
            return
        config = self.controller.config
        receivedFilesDir = os.path.expanduser(config.user['receivedFilesDir'])
        if not os.path.exists(receivedFilesDir):
            print receivedFilesDir + ' does not exist. ' \
                'Saving files to home directory.'
            receivedFilesDir = os.path.expanduser('~/')
        if config.user['receivedFilesSortedByUser']:
            receivedFilesDirSub = os.path.join(receivedFilesDir, p2p.mail)
            if os.path.exists(receivedFilesDirSub):
                receivedFilesDir = receivedFilesDirSub
            else:
                os.mkdir(receivedFilesDirSub)
                if os.path.exists(receivedFilesDirSub):
                    receivedFilesDir = receivedFilesDirSub
        name = os.path.join(receivedFilesDir, self.getFilename())
        num = 0
        numstr = ''
        while os.path.exists(name + numstr):
            num += 1
            numstr = '.' + str(num)
        self.localPath = name + numstr
        dest = open(self.localPath, 'wb')
        src.seek(0)
        buffer = src.read(32 * 1024)
        while buffer:
            dest.write(buffer)
            buffer = src.read(32 * 1024)
        dest.close()
        src.close()
    def open(self):
        '''open received file'''
        if self.localPath != None:
            try:
                desktop.open(self.localPath)
            except OSError:
                pass
    def getFilename(self):
        return self.context.filename
    def setBytesReceived(self, bytes):
        self.receivedBytes = bytes
    def getFraction(self):
        '''return the progress of this transfer as a float from 0 to 1'''
        if self.context.file_size:
            return float(self.receivedBytes) / float(self.context.file_size)
        else:
            return 1
