import os
import sys
import logging
import servicemanager
try:
    servicemanager.LogInfoMsg(os.getcwd())
    servicemanager.LogInfoMsg(__file__)
    servicemanager.LogInfoMsg(sys.argv[0])
except:
    pass
class ServiceEventLogHandler(logging.Handler):
    """Dispatches logging events to the win32 services event log.
    Requires pywin32.    
    """
    import servicemanager
    def emit(self, record):
        """Emit a record.
        If a formatter is specified, it is used to format the record.
        This record is then written to the win32 services event log,
        with the type set to the appropriate type based on the level.
        """
        try:
            servicemgr = self.servicemanager
            level = record.levelno
            msg = self.format(record)
            if level >= logging.ERROR:
                servicemgr.LogErrorMsg(msg)
            elif level >= logging.WARNING:
                servicemgr.LogWarningMsg(msg)
            elif level >= logging.INFO:
                servicemgr.LogInfoMsg(msg)
            elif level >= logging.DEBUG:
                pass
            else:
                pass
        except:
            self.handleError(record)
    def handleError(self, record):
        """
        Handle errors which occur during an emit() call.
        sys.stderr does nowwhere, so redirect this into the event log, too.
        """
        if raiseExceptions:
            try:
                import cStringIO as StringIO
            except ImportError:
                import StringIO
            import traceback
            ei = sys.exc_info()
            msg = StringIO.StringIO()
            traceback.print_exception(ei[0], ei[1], ei[2], None, msg)
            msg.seek(0)
            self.servicemanager.LogErrorMsg(msg)
            del ei
class ServiceEventLogHandlerWrapper(object):
    """Pretend that the ServiceEventLogHandler is a file-like object,
    so we can use it while we don't use the proper logging module."""
    def __init__(self, service_name, level=logging.INFO):
        self.log = ServiceEventLogHandler()
        self.name = service_name
        self.level = level
        self.data = ""
    def write(self, data):
        self.data += data
        if '\n' not in data:
            return
        if not self.data.strip():
            return
        record = logging.LogRecord(self.name, self.level, "", "",
                                   self.data, None, None)
        self.log.emit(record)
        self.data = ""
import win32api
try:
    win32api.GetConsoleTitle()
except win32api.error:
    if hasattr(sys, "frozen"):
        sys.stdout = ServiceEventLogHandlerWrapper("pop3proxy")
        sys.stderr = ServiceEventLogHandlerWrapper("pop3proxy",
                                                   logging.ERROR)
    else:
        import win32traceutil
if not hasattr(sys, "frozen"):
    this_filename = __file__
    sb_dir = os.path.dirname(os.path.dirname(this_filename))
    sb_scripts_dir = os.path.join(sb_dir,"scripts")
    sys.path.insert(0, sb_dir)
    sys.path.insert(-1, sb_scripts_dir)
    if os.path.exists(os.path.join(sb_dir, "SpamBayesData")):
        os.chdir(os.path.join(sb_dir, "SpamBayesData"))
    else:
        os.chdir(sb_dir)
    from win32com.shell import shell, shellcon
    sys32path = shell.SHGetFolderPath(0, shellcon.CSIDL_SYSTEM, 0, 0)
    for path in sys.path[:-1]:
        if path == sys32path:
            sys.path.remove(path)
            assert path not in sys.path, \
                   "Please remove multiple copies of windows\system32 in path"
            sys.path.append(path) # put it at the *end*
    del sys32path
    del shell
    del shellcon
    del path
import traceback
import threading
import cStringIO
import sb_server
import win32serviceutil, win32service
import pywintypes, win32con, winerror
from ntsecuritycon import *
class Service(win32serviceutil.ServiceFramework):
    _svc_name_ = "pop3proxy"
    _svc_display_name_ = "SpamBayes Service"
    _svc_deps_ =  ['tcpip'] # We depend on the tcpip service.
    def __init__(self, args):
        win32serviceutil.ServiceFramework.__init__(self, args)
        self.event_stopped = threading.Event()
        self.event_stopping = threading.Event()
        self.thread = None
    def SvcStop(self):
        self.ReportServiceStatus(win32service.SERVICE_STOP_PENDING)
        self.event_stopping.set()
        sb_server.stop()
    def SvcDoRun(self):
        import servicemanager
        try:
            sb_server.prepare(can_stop=False)
        except sb_server.AlreadyRunningException:
            msg = "The SpamBayes proxy service could not be started, as "\
                  "another SpamBayes server is already running on this machine"
            servicemanager.LogErrorMsg(msg)
            errCode = winerror.ERROR_SERVICE_SPECIFIC_ERROR
            self.ReportServiceStatus(win32service.SERVICE_STOPPED,
                                     win32ExitCode=errCode, svcExitCode=1)
            return
        assert not sb_server.state.launchUI, "Service can't launch a UI"
        thread = threading.Thread(target=self.ServerThread)
        thread.start()
        from spambayes.Options import optionsPathname
        extra = " as user '%s', using config file '%s'" \
                % (win32api.GetUserName(),
                   optionsPathname)
        servicemanager.LogMsg(
            servicemanager.EVENTLOG_INFORMATION_TYPE,
            servicemanager.PYS_SERVICE_STARTED,
            (self._svc_name_, extra)
            )
        try:
            self.event_stopping.wait()
            for i in range(60):
                self.ReportServiceStatus(win32service.SERVICE_STOP_PENDING)
                self.event_stopped.wait(1)
                if self.event_stopped.isSet():
                    break
                print "The service is still shutting down..."
            else:
                print "The worker failed to stop - aborting it anyway"
        except KeyboardInterrupt:
            pass
        s = sb_server.state
        status = " after %d sessions (%d ham, %d spam, %d unsure)" % \
                (s.totalSessions, s.numHams, s.numSpams, s.numUnsure)
        servicemanager.LogMsg(
            servicemanager.EVENTLOG_INFORMATION_TYPE,
            servicemanager.PYS_SERVICE_STOPPED,
            (self._svc_name_, status)
            )
    def ServerThread(self):
        try:
            try:
                sb_server.start()
            except SystemExit:
                print "pop3proxy service shutting down due to user request"
            except:
                ob = cStringIO.StringIO()
                traceback.print_exc(file=ob)
                message = "The pop3proxy service failed with an " \
                          "unexpected error\r\n\r\n" + ob.getvalue()
                print message
                import servicemanager
                servicemanager.LogErrorMsg(message)
        finally:
            self.event_stopping.set()
            self.event_stopped.set()
if __name__=='__main__':
    if "install" in sys.argv:
        from spambayes.Options import optionsPathname
        if not os.path.exists(optionsPathname):
            data_directory = os.path.join(os.path.dirname(sys.argv[0]),
                                          "..", "SpamBayesData")
            data_directory = os.path.abspath(data_directory)
            if not os.path.exists(data_directory):
                print "Creating data directory at", data_directory
                os.makedirs(data_directory)
    win32serviceutil.HandleCommandLine(Service)
