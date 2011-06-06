"""
This module provides support for Twisted to interact with the glib/gtk2 mainloop.
In order to use this support, simply do the following::
    |  from twisted.internet import gtk2reactor
    |  gtk2reactor.install()
Then use twisted.internet APIs as usual.  The other methods here are not
intended to be called directly.
When installing the reactor, you can choose whether to use the glib
event loop or the GTK+ event loop which is based on it but adds GUI
integration.
API Stability: stable
Maintainer: U{Itamar Shtull-Trauring<mailto:twisted@itamarst.org>}
"""
__all__ = ['install']
import sys
from zope.interface import implements
try:
    if not hasattr(sys, 'frozen'):
        import pygtk
        pygtk.require('2.0')
except (ImportError, AttributeError):
    pass # maybe we're using pygtk before this hack existed.
import gobject
if hasattr(gobject, "threads_init"):
    gobject.threads_init()
from twisted.python import log, threadable, runtime, failure, components
from twisted.internet.interfaces import IReactorFDSet
from twisted.internet import main, posixbase, error, selectreactor
reads = {}
writes = {}
hasReader = reads.has_key
hasWriter = writes.has_key
_simtag = None
POLL_DISCONNECTED = gobject.IO_HUP | gobject.IO_ERR | gobject.IO_NVAL
INFLAGS = gobject.IO_IN | POLL_DISCONNECTED
OUTFLAGS = gobject.IO_OUT | POLL_DISCONNECTED
def _our_mainquit():
    import gtk
    if gtk.main_level():
        gtk.main_quit()
class Gtk2Reactor(posixbase.PosixReactorBase):
    """GTK+-2 event loop reactor.
    """
    implements(IReactorFDSet)
    def __init__(self, useGtk=True):
        self.context = gobject.main_context_default()
        self.loop = gobject.MainLoop()
        posixbase.PosixReactorBase.__init__(self)
        if (hasattr(gobject, "pygtk_version") and gobject.pygtk_version >= (2, 3, 91)
            and not useGtk):
            self.__pending = self.context.pending
            self.__iteration = self.context.iteration
            self.__crash = self.loop.quit
            self.__run = self.loop.run
        else:
            import gtk
            self.__pending = gtk.events_pending
            self.__iteration = gtk.main_iteration
            self.__crash = _our_mainquit
            self.__run = gtk.main
    def input_add(self, source, condition, callback):
        if hasattr(source, 'fileno'):
            def wrapper(source, condition, real_s=source, real_cb=callback):
                return real_cb(real_s, condition)
            return gobject.io_add_watch(source.fileno(), condition, wrapper)
        else:
            return gobject.io_add_watch(source, condition, callback)
    def addReader(self, reader):
        if not hasReader(reader):
            reads[reader] = self.input_add(reader, INFLAGS, self.callback)
    def addWriter(self, writer):
        if not hasWriter(writer):
            writes[writer] = self.input_add(writer, OUTFLAGS, self.callback)
    def removeAll(self):
        return self._removeAll(reads, writes)
    def removeReader(self, reader):
        if hasReader(reader):
            gobject.source_remove(reads[reader])
            del reads[reader]
    def removeWriter(self, writer):
        if hasWriter(writer):
            gobject.source_remove(writes[writer])
            del writes[writer]
    doIterationTimer = None
    def doIterationTimeout(self, *args):
        self.doIterationTimer = None
        return 0 # auto-remove
    def doIteration(self, delay):
        log.msg(channel='system', event='iteration', reactor=self)
        if self.__pending():
            self.__iteration(0)
            return
        if delay == 0:
            return # shouldn't delay, so just return
        self.doIterationTimer = gobject.timeout_add(int(delay * 1000),
                                                self.doIterationTimeout)
        self.__iteration(1) # block
        if self.doIterationTimer:
            gobject.source_remove(self.doIterationTimer)
            self.doIterationTimer = None
    def crash(self):
        self.__crash()
    def run(self, installSignalHandlers=1):
        self.startRunning(installSignalHandlers=installSignalHandlers)
        gobject.timeout_add(0, self.simulate)
        self.__run()
    def _doReadOrWrite(self, source, condition, faildict={
        error.ConnectionDone: failure.Failure(error.ConnectionDone()),
        error.ConnectionLost: failure.Failure(error.ConnectionLost()),
        }):
        why = None
        didRead = None
        if condition & POLL_DISCONNECTED and \
               not (condition & gobject.IO_IN):
            why = main.CONNECTION_LOST
        else:
            try:
                if condition & gobject.IO_IN:
                    why = source.doRead()
                    didRead = source.doRead
                if not why and condition & gobject.IO_OUT:
                    if not source.disconnected and source.doWrite != didRead:
                        why = source.doWrite()
                        didRead = source.doWrite # if failed it was in write
            except:
                why = sys.exc_info()[1]
                log.msg('Error In %s' % source)
                log.deferr()
        if why:
            self._disconnectSelectable(source, why, didRead == source.doRead)
    def callback(self, source, condition):
        log.callWithLogger(source, self._doReadOrWrite, source, condition)
        self.simulate() # fire Twisted timers
        return 1 # 1=don't auto-remove the source
    def simulate(self):
        """Run simulation loops and reschedule callbacks.
        """
        global _simtag
        if _simtag is not None:
            gobject.source_remove(_simtag)
        self.runUntilCurrent()
        timeout = min(self.timeout(), 0.1)
        if timeout is None:
            timeout = 0.1
        _simtag = gobject.timeout_add(int(timeout * 1010), self.simulate)
components.backwardsCompatImplements(Gtk2Reactor)
class PortableGtkReactor(selectreactor.SelectReactor):
    """Reactor that works on Windows.
    input_add is not supported on GTK+ for Win32, apparently.
    """
    def crash(self):
        import gtk
        if hasattr(gtk, 'main_quit'):
            gtk.main_quit()
        else:
            gtk.mainquit()
    def run(self, installSignalHandlers=1):
        import gtk
        self.startRunning(installSignalHandlers=installSignalHandlers)
        self.simulate()
        if hasattr(gtk, 'main'):
            gtk.main()
        else:
            gtk.mainloop()
    def simulate(self):
        """Run simulation loops and reschedule callbacks.
        """
        global _simtag
        if _simtag is not None:
            gobject.source_remove(_simtag)
        self.iterate()
        timeout = min(self.timeout(), 0.1)
        if timeout is None:
            timeout = 0.1
        _simtag = gobject.timeout_add(int(timeout * 1010), self.simulate)
def install(useGtk=True):
    """Configure the twisted mainloop to be run inside the gtk mainloop.
    @param useGtk: should glib rather than GTK+ event loop be
    used (this will be slightly faster but does not support GUI).
    """
    reactor = Gtk2Reactor(useGtk)
    from twisted.internet.main import installReactor
    installReactor(reactor)
    return reactor
def portableInstall(useGtk=True):
    """Configure the twisted mainloop to be run inside the gtk mainloop.
    """
    reactor = PortableGtkReactor()
    from twisted.internet.main import installReactor
    installReactor(reactor)
    return reactor
if runtime.platform.getType() != 'posix':
    install = portableInstall
