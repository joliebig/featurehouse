"""A script to provide an icon in the Windows taskbar tray to control the
POP3 proxy.
"""

__author__ = "Tony Meyer <ta-meyer@ihug.co.nz>, Adam Walker"

__credits__ = "Mark Hammond, all the Spambayes folk."

try:

    True, False

except NameError:

    True, False = 1, 0



import os

import sys

import webbrowser

import thread

import traceback

verbose = 0

def SetWaitCursor(wait):

    import win32gui, win32con

    if wait:

        hCursor = win32gui.LoadCursor(0, win32con.IDC_WAIT)

    else:

        hCursor = win32gui.LoadCursor(0, 0)

    win32gui.SetCursor(hCursor)


import win32con

import winerror

from win32api import *

from win32gui import *

from win32api import error as win32api_error

from win32service import *

try:

    GetConsoleTitle()

except win32api_error:

    if hasattr(sys, "frozen"):

        temp_dir = GetTempPath()

        for i in range(3,0,-1):

            try: os.unlink(os.path.join(temp_dir, "SpamBayesServer%d.log" % (i+1)))

            except os.error: pass

            try:

                os.rename(
                    os.path.join(temp_dir, "SpamBayesServer%d.log" % i),
                    os.path.join(temp_dir, "SpamBayesServer%d.log" % (i+1))
                    )

            except os.error: pass

        sys.stdout = open(os.path.join(temp_dir,"SpamBayesServer1.log"), "wt", 0)

        sys.stderr = sys.stdout

    else:

        import win32traceutil

 try:

    if hasattr(sys, "frozen"):

        if sys.frozen == "dll":

            this_filename = win32api.GetModuleFileName(sys.frozendllhandle)

        else:

            this_filename = os.path.abspath(sys.argv[0])

    else:

        this_filename = os.path.abspath(__file__)

except NameError: 

    this_filename = os.path.abspath(sys.argv[0])



this_dir = os.path.dirname(this_filename)

if not hasattr(sys, "frozen"):

    sys.path.insert(-1, this_dir)

    sys.path.insert(-1, os.path.dirname(this_dir))

    sys.path.insert(-1, os.path.join(os.path.dirname(this_dir),"scripts"))



import sb_server

from spambayes import Dibbler

from spambayes.Options import options

WM_TASKBAR_NOTIFY = win32con.WM_USER + 20

START_STOP_ID = 1024

runningStatus = (SERVICE_START_PENDING, SERVICE_RUNNING, SERVICE_CONTINUE_PENDING)

stoppedStatus = (SERVICE_PAUSED, SERVICE_STOP_PENDING, SERVICE_STOPPED)

serviceName = "pop3proxy"

def IsServerRunningAnywhere():

    import win32event

    mutex_name = "SpamBayesServer"

    try:

        hmutex = win32event.CreateMutex(None, True, mutex_name)

        try:

            return GetLastError()==winerror.ERROR_ALREADY_EXISTS

        finally:

            hmutex.Close()

    except win32event.error, details:

        if details[0] != winerror.ERROR_ACCESS_DENIED:

            raise

        return True
 class  MainWindow (object) :
	def __init__(self):

        self.control_functions = {START_STOP_ID : ("Stop SpamBayes", self.Stop),
                                  1025 : ("-", None),
                                  1026 : ("Review messages ...", self.OpenReview),
                                  1027 : ("View information ...", self.OpenInterface),
                                  1028 : ("Configure ...", self.OpenConfig),
                                  1029 : ("Check for latest version", self.CheckVersion),
                                  1030 : ("-", None),
                                  1031 : ("Help", self.GetHelp),
                                  1032 : ("-", None),
                                  1099 : ("Exit SpamBayes", self.OnExit),
                                  }

        msg_TaskbarRestart = RegisterWindowMessage("TaskbarCreated");

        message_map = {
            msg_TaskbarRestart: self.OnTaskbarRestart,
            win32con.WM_DESTROY: self.OnDestroy,
            win32con.WM_COMMAND: self.OnCommand,
            WM_TASKBAR_NOTIFY : self.OnTaskbarNotify,
        }

        self.have_prepared_state = False

        self.last_started_state = None

        self.use_service = \
                GetVersionEx()[3]==win32con.VER_PLATFORM_WIN32_NT

        hinst = GetModuleHandle(None)

        dialogTemplate = [['SpamBayes', (14, 10, 246, 187),
                           -1865809852 & ~win32con.WS_VISIBLE, None,
                           (8, 'Tahoma')],]

        self.hwnd = CreateDialogIndirect(hinst, dialogTemplate, 0,
                                         message_map)

        startedIconPathName = "%s\\..\\windows\\resources\\sb-started.ico" % \
                       (os.path.dirname(sb_server.__file__),)

        stoppedIconPathName = "%s\\..\\windows\\resources\\sb-stopped.ico" % \
                       (os.path.dirname(sb_server.__file__),)

        if hasattr(sys, "frozen"):

            self.hstartedicon = self.hstoppedicon = None

            hexe = GetModuleHandle(None)

            icon_flags = win32con.LR_DEFAULTSIZE

            self.hstartedicon = LoadImage(hexe, 1000, win32con.IMAGE_ICON,
                                          16, 16, icon_flags)

            self.hstoppedicon = LoadImage(hexe, 1010, win32con.IMAGE_ICON,
                                          16, 16, icon_flags)

        else:

            icon_flags = win32con.LR_LOADFROMFILE | win32con.LR_DEFAULTSIZE

            self.hstartedicon = LoadImage(hinst, startedIconPathName, win32con.IMAGE_ICON,
                                          16, 16, icon_flags)

            self.hstoppedicon = LoadImage(hinst, stoppedIconPathName, win32con.IMAGE_ICON,
                                          16, 16, icon_flags)

        self._AddTaskbarIcon()

        self.started = IsServerRunningAnywhere()

        self.tip = None

        if self.use_service and not self.IsServiceAvailable():

            print "Service not available. Using thread."

            self.use_service = False

        if not self.started:

            self.Start()

        else:

            print "The server is already running externally - not starting " \
                  "a local server"
 def _AddTaskbarIcon(self):

        flags = NIF_ICON | NIF_MESSAGE | NIF_TIP

        nid = (self.hwnd, 0, flags, WM_TASKBAR_NOTIFY, self.hstartedicon,
            "SpamBayes")

        try:

            Shell_NotifyIcon(NIM_ADD, nid)

        except win32api_error:

            print "Ignoring error adding taskbar icon - explorer may not " \
                  "be running (yet)."
 def BuildToolTip(self):

        tip = None

        if self.started:

            if self.use_service:

                tip = "SpamBayes running."

            else:

                tip = "SpamBayes %i spam %i ham %i unsure %i sessions %i active" %\
                      (sb_server.state.numSpams, sb_server.state.numHams,
                       sb_server.state.numUnsure, sb_server.state.totalSessions,
                       sb_server.state.activeSessions)

        else:

            tip = "SpamBayes is not running"

        return tip
 def UpdateIcon(self):

        flags = NIF_TIP | NIF_ICON

        if self.started:

            hicon = self.hstartedicon

        else:

            hicon = self.hstoppedicon

        self.tip = self.BuildToolTip()

        nid = (self.hwnd, 0, flags, WM_TASKBAR_NOTIFY, hicon, self.tip)

        if self.started:

            self.control_functions[START_STOP_ID] = ("Stop SpamBayes",
                                                     self.Stop)

        else:

            self.control_functions[START_STOP_ID] = ("Start SpamBayes",
                                         self.Start)

        Shell_NotifyIcon(NIM_MODIFY, nid)
 def IsServiceAvailable(self):

        try:

            schSCManager = OpenSCManager(None, None, SC_MANAGER_CONNECT)

            schService   = OpenService(schSCManager, serviceName,
                                       SERVICE_QUERY_STATUS)

            if schService:

                CloseServiceHandle(schService)

            return schService != None

        except win32api_error, details:

            if details[0] != winerror.ERROR_SERVICE_DOES_NOT_EXIST:

                print "Unexpected windows error querying for service"

                print details

            return False
 def GetServiceStatus(self):

        schSCManager = OpenSCManager(None, None, SC_MANAGER_CONNECT)

        schService   = OpenService(schSCManager, serviceName,
                                   SERVICE_QUERY_STATUS)

        ssStatus     = QueryServiceStatus(schService)

        CloseServiceHandle(schService)

        return ssStatus[1]
 def StartService(self):

        schSCManager = OpenSCManager(None, None, SC_MANAGER_CONNECT)

        schService   = OpenService(schSCManager, serviceName, SERVICE_START |
                                   SERVICE_QUERY_STATUS)

        ssStatus     = QueryServiceStatus(schService)

        if ssStatus[1] in runningStatus:

            self.started = True

            CloseServiceHandle(schService)

            return

        StartService(schService, None)

        ssStatus         = QueryServiceStatus(schService)

        dwStartTickCount = GetTickCount()

        dwOldCheckPoint  = ssStatus[5]

        while ssStatus[1] == SERVICE_START_PENDING:

            dwWaitTime = ssStatus[6] / 10;

            if dwWaitTime < 1000:

                dwWaitTime = 1000

            elif dwWaitTime > 10000:

                dwWaitTime = 10000

            Sleep(dwWaitTime);

            ssStatus = QueryServiceStatus(schService)

            if ssStatus[5] > dwOldCheckPoint:

                dwStartTickCount = GetTickCount()

                dwOldCheckPoint = ssStatus[5]

            else:

                if GetTickCount() - dwStartTickCount > ssStatus[6]:

                    break

        self.started = ssStatus[1] == SERVICE_RUNNING

        CloseServiceHandle(schService)

        self.started = True
 def StopService(self):

        schSCManager = OpenSCManager(None, None, SC_MANAGER_CONNECT)

        schService   = OpenService(schSCManager, serviceName, SERVICE_STOP |
                                   SERVICE_QUERY_STATUS)

        ssStatus     = QueryServiceStatus(schService)

        if ssStatus[1] in stoppedStatus:

            self.started = False

            CloseServiceHandle(schService)

            return

        ControlService(schService, SERVICE_CONTROL_STOP)

        ssStatus         = QueryServiceStatus(schService)

        dwStartTickCount = GetTickCount()

        dwOldCheckPoint  = ssStatus[5]

        while ssStatus[1] == SERVICE_STOP_PENDING:

            dwWaitTime = ssStatus[6] / 10;

            if dwWaitTime < 1000:

                dwWaitTime = 1000

            elif dwWaitTime > 10000:

                dwWaitTime = 10000

            Sleep(dwWaitTime);

            ssStatus = QueryServiceStatus(schService)

            if ssStatus[5] > dwOldCheckPoint:

                dwStartTickCount = GetTickCount()

                dwOldCheckPoint = ssStatus[5]

            else:

                if GetTickCount() - dwStartTickCount > ssStatus[6]:

                    break

        CloseServiceHandle(schService)

        self.started = False
 def OnDestroy(self, hwnd, msg, wparam, lparam):

        nid = (self.hwnd, 0)

        Shell_NotifyIcon(NIM_DELETE, nid)

        PostQuitMessage(0)
 def OnTaskbarNotify(self, hwnd, msg, wparam, lparam):

        if lparam==win32con.WM_MOUSEMOVE:

            if self.tip != self.BuildToolTip():

                self.UpdateIcon()

            else:

                self.CheckCurrentState()

        if lparam==win32con.WM_LBUTTONUP:

            pass

        elif lparam==win32con.WM_LBUTTONDBLCLK:

            self.OpenReview()

        elif lparam==win32con.WM_RBUTTONUP:

            self.CheckCurrentState()

            menu = CreatePopupMenu()

            ids = self.control_functions.keys()

            ids.sort()

            for id in ids:

                (wording, function) = self.control_functions[id]

                if function:

                    AppendMenu( menu, win32con.MF_STRING, id, wording)

                else:

                    AppendMenu( menu, win32con.MF_SEPARATOR, id, wording)

            pos = GetCursorPos()

            SetForegroundWindow(self.hwnd)

            try:

                SetMenuDefaultItem(menu, 2, 1)

            except NameError:

                pass

            TrackPopupMenu(menu, win32con.TPM_LEFTALIGN, pos[0], pos[1], 0,
                           self.hwnd, None)

            PostMessage(self.hwnd, win32con.WM_NULL, 0, 0)

        return 1
 def OnCommand(self, hwnd, msg, wparam, lparam):

        id = LOWORD(wparam)

        try:

            unused, function = self.control_functions[id]

        except KeyError:

            print "Unknown command -", id

            return

        function()
 def OnTaskbarRestart(self, hwnd, msg, wparam, lparam):

        self._AddTaskbarIcon()
 def OnExit(self):

        if self.started and not self.use_service:

            try:

                sb_server.stop()

            except:

                print "Error stopping proxy at shutdown"

                traceback.print_exc()

                print "Shutting down anyway..."

            self.started = False

        DestroyWindow(self.hwnd)

        PostQuitMessage(0)
 def _ProxyThread(self):

        self.started = True

        try:

            sb_server.start()

        finally:

            self.started = False

            self.have_prepared_state = False
 def StartProxyThread(self):

        thread.start_new_thread(self._ProxyThread, ())
 def Start(self):

        self.CheckCurrentState()

        if self.started:

            print "Ignoring start request - server already running"

            return

        if self.use_service:

            if verbose: print "Doing 'Start' via service"

            if self.GetServiceStatus() in stoppedStatus:

                self.StartService()

            else:

                print "Service was already running - ignoring!"

        else:

            if verbose: print "Doing 'Start' internally"

            if not self.have_prepared_state:

                try:

                    sb_server.prepare()

                    self.have_prepared_state = True

                except sb_server.AlreadyRunningException:

                    msg = "The proxy is already running on this " \
                          "machine - please\r\n stop the existing " \
                          "proxy, and try again."

                    self.ShowMessage(msg)

                    return

            self.StartProxyThread()

        self.started = True

        self.UpdateUIState()
 def Stop(self):

        self.CheckCurrentState()

        if not self.started:

            print "Ignoring stop request - server doesn't appear to be running"

            return

        try:

            use_service = self.use_service

            if use_service:

                if verbose: print "Doing 'Stop' via service"

                if self.GetServiceStatus() not in stoppedStatus:

                    self.StopService()

                else:

                    print "Service was already stopped - weird - falling " \
                          "back to a socket based quit"

                    use_service = False

            if not use_service:

                if verbose: print "Stopping local server"

                sb_server.stop()

        except:

            print "There was an error stopping the server"

            traceback.print_exc()

        self.started = False

        self.UpdateUIState()
 def CheckCurrentState(self):

        self.started = IsServerRunningAnywhere()

        self.UpdateUIState()
 def UpdateUIState(self):

        if self.started != self.last_started_state:

            self.UpdateIcon()

            if self.started:

                self.control_functions[START_STOP_ID] = ("Stop SpamBayes",
                                                         self.Stop)

            else:

                self.control_functions[START_STOP_ID] = ("Start SpamBayes",
                                             self.Start)

            self.last_started_state = self.started
 def OpenInterface(self):

        if self.started:

            webbrowser.open_new("http://localhost:%d/" % \
                                (options["html_ui", "port"],))

        else:

            self.ShowMessage("SpamBayes is not running.")
 def OpenConfig(self):

        if self.started:

            webbrowser.open_new("http://localhost:%d/config" % \
                                (options["html_ui", "port"],))

        else:

            self.ShowMessage("SpamBayes is not running.")
 def OpenReview(self):

        if self.started:

            webbrowser.open_new("http://localhost:%d/review" % \
                                (options["html_ui", "port"],))

        else:

            self.ShowMessage("SpamBayes is not running.")
 def GetHelp(self):

        self.ShowHTML("troubleshooting.html")
 def ShowHTML(self, url):

        """Displays the main SpamBayes documentation in your Web browser"""

        import sys, os, urllib

        if urllib.splittype(url)[0] is None: 

            if hasattr(sys, "frozen"):

                fname = os.path.join(os.path.dirname(sys.argv[0]),
                                     "..", "docs", "sb_server", url)

                if not os.path.isfile(fname):

                    fname = os.path.join(os.path.dirname(sys.argv[0]), url)

            else:

                fname = os.path.join(os.path.dirname(__file__), "docs", url)

            fname = os.path.abspath(fname)

            if not os.path.isfile(fname):

                self.ShowMessage("Can't find "+url)

                return

            url = fname

        SetWaitCursor(1)

        os.startfile(url)

        SetWaitCursor(0)
 def CheckVersion(self):

        from spambayes.Version import get_current_version, get_version, \
                get_download_page, fetch_latest_dict

        app_name = "POP3 Proxy"

        app_display_name = "SpamBayes POP3 Proxy"

        ver_current = get_current_version()

        cur_ver_string = ver_current.get_long_version(app_display_name)

        try:

            SetWaitCursor(1)

            latest = fetch_latest_dict()

            SetWaitCursor(0)

            ver_latest = get_version(app_name, version_dict=latest)

            latest_ver_string = ver_latest.get_long_version(app_display_name)

        except:

            self.ShowMessage("Error checking the latest version")

            traceback.print_exc()

            return

        ver_message = "Current version is %s.\r\n" \
                      "Latest version is %s.\r\n\r\n" % \
                      (cur_ver_string, latest_ver_string)

        if ver_latest == ver_current:

            ver_message += "Your are running the latest downloadable version."

        elif ver_current > ver_latest:

            ver_message += "Your current version is newer than the latest " \
                           "downloadable version."

        else:

            ver_message += "There is a newer version available.  You may " \
                           "download the updated version from:\r\n"

            url = get_download_page(app_name, version_dict=latest)

            ver_message += url

        self.ShowMessage(ver_message)
 def ShowMessage(self, msg):

        MessageBox(self.hwnd, msg, "SpamBayes", win32con.MB_OK)

def main():

    w = MainWindow()

    PumpMessages()
 if __name__=='__main__':

    main()

 if __name__=='__main__':

    main()



try:

    True, False

except NameError:

    True, False = 1, 0



try:

    GetConsoleTitle()

except win32api_error:

    if hasattr(sys, "frozen"):

        temp_dir = GetTempPath()

        for i in range(3,0,-1):

            try: os.unlink(os.path.join(temp_dir, "SpamBayesServer%d.log" % (i+1)))

            except os.error: pass

            try:

                os.rename(
                    os.path.join(temp_dir, "SpamBayesServer%d.log" % i),
                    os.path.join(temp_dir, "SpamBayesServer%d.log" % (i+1))
                    )

            except os.error: pass

        sys.stdout = open(os.path.join(temp_dir,"SpamBayesServer1.log"), "wt", 0)

        sys.stderr = sys.stdout

    else:

        import win32traceutil

 try:

    if hasattr(sys, "frozen"):

        if sys.frozen == "dll":

            this_filename = win32api.GetModuleFileName(sys.frozendllhandle)

        else:

            this_filename = os.path.abspath(sys.argv[0])

    else:

        this_filename = os.path.abspath(__file__)

except NameError: 

    this_filename = os.path.abspath(sys.argv[0])



if not hasattr(sys, "frozen"):

    sys.path.insert(-1, this_dir)

    sys.path.insert(-1, os.path.dirname(this_dir))

    sys.path.insert(-1, os.path.join(os.path.dirname(this_dir),"scripts"))



