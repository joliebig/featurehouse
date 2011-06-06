"""Support for waiting for processes using 63 waits per thread
to avoid Windows limitations to *WaitForMultipleObjects*.
API Stability: unstable
Maintainer: U{Justin Johnson<mailto:justinjohnson@gmail.com>}
"""
import win32api
import win32gui
import win32con
import win32file
import win32pipe
import win32process
import win32security
from win32event import CreateEvent, SetEvent, WaitForSingleObject
from win32event import MsgWaitForMultipleObjects, WAIT_OBJECT_0
from win32event import WAIT_TIMEOUT, INFINITE, QS_ALLINPUT, QS_POSTMESSAGE
from win32event import QS_ALLEVENTS
from zope.interface import implements
from twisted.internet import error
from twisted.python import failure, components
from twisted.internet.interfaces import IProcessTransport
import ops
import os
import sys
import re
import time
import threading
import itertools
counter = itertools.count(1)
WM_NEW_PHANDLE = win32con.WM_USER + 1
WM_CLOSE_THREAD = win32con.WM_USER + 2
class ProcessWaiter(object):
    """Waiter
    """
    usedThreads = []  # threads waiting on 63 process handles
    availableThreads = [] # threads waiting on less than 63 process handles
    threadToNumProcessHandles = {}
    threadToMsgWindowCreationEvent = {} # event signalled when msg window created
    threadToMsgWindowCreated = {} # boolean indicated msg window is created
    needWaiting = {} # used to pass process handles to new WFMO thread
    phandleToTransport = {} # so we can call methods on the transport when a proc handle is signalled
    threadToMsgWindow = {} # since we need the window to call PostMessage
    phandleKeyToThreadHandle = {} # proc handle keys passed to PostThreadMessage to tell thread to wait on new proc handle
    phandleToPhandleKey = {}
    threadToNumEnded = {}
    def __init__(self, reactor):
        def stopThreads():
            for t in self.threadToMsgWindow.keys():
                if t.isAlive() and self.threadToMsgWindowCreated[t]:
                    win32api.PostMessage(
                        self.threadToMsgWindow[t], # thread id
                        WM_CLOSE_THREAD, # message 
                        0, # wParam
                        0 # lParam
                        )
        reactor.addSystemEventTrigger("before", "shutdown", stopThreads)
    def beginWait(self, reactor, processHandle, processTransport):
        self.reactor = reactor
        processHandleKey = counter.next()
        self.phandleToPhandleKey[processHandle] = processHandleKey
        self.phandleToTransport[processHandle] = processTransport
        self.needWaiting[processHandleKey] = processHandle
        self.realPid = os.getpid()
        self.notifyOnExit(processHandle, processTransport)
    def notifyOnExit(self, processHandle, processTransport):
        processHandleKey = self.phandleToPhandleKey[processHandle]
        if len(self.availableThreads) > 0:
            wfmoThread = self.availableThreads[0]
            self.threadToNumProcessHandles[wfmoThread] += 1
            self.phandleKeyToThreadHandle[processHandleKey] = wfmoThread
            if self.threadToNumProcessHandles[wfmoThread] == 63:
                self.usedThreads.append(wfmoThread)
                self.availableThreads.remove(wfmoThread)
            if self.threadToMsgWindowCreated[wfmoThread] is False:
                val = WaitForSingleObject(self.threadToMsgWindowCreationEvent[wfmoThread], INFINITE)
                if val != WAIT_OBJECT_0:
                    raise RuntimeError("WaitForSingleObject returned %d.  It should only return %d" % (val, WAIT_OBJECT_0))
            if win32api.PostMessage(
                    self.threadToMsgWindow[wfmoThread],
                    WM_NEW_PHANDLE, # message 
                    processHandleKey, # wParam
                    0 # lParam
                    ) == 0:
                raise Exception("Failed to post thread message!")
        else:
            wfmoThread = threading.Thread(
                    target=self.doWaitForProcessExit,
                    args=(processHandleKey,),
                    name="iocpreactor.process_waiter.ProcessWaiter.waitForProcessExit pid=%d" % self.realPid)
            self.threadToMsgWindowCreationEvent[wfmoThread] = CreateEvent(None, 0, 0, None)
            self.threadToMsgWindowCreated[wfmoThread] = False
            self.threadToNumProcessHandles[wfmoThread] = 1
            self.availableThreads.append(wfmoThread)
            self.phandleKeyToThreadHandle[processHandleKey] = wfmoThread
            wfmoThread.start()
    def doWaitForProcessExit(self, processHandleKey):
        theWindow = win32gui.CreateWindow("Button", # lpClassName
                                          "",       # lpWindowName
                                          0,        # dwStyle
                                          0,        # x
                                          0,        # y
                                          0,        # width
                                          0,        # height
                                          0,        # parent
                                          0,        # menu
                                          0,        # hInstance
                                          None      # lParam
                                          )
        handles = []
        handles.append(self.needWaiting[processHandleKey])
        threadHandle = self.phandleKeyToThreadHandle[processHandleKey]
        self.threadToMsgWindow[threadHandle] = theWindow
        self.threadToNumEnded[threadHandle] = 0
        self.threadToMsgWindowCreated[threadHandle] = True
        SetEvent(self.threadToMsgWindowCreationEvent[threadHandle])
        while True:
            val = MsgWaitForMultipleObjects(handles, 0, INFINITE, QS_POSTMESSAGE | QS_ALLEVENTS)
            if val >= WAIT_OBJECT_0 and val < WAIT_OBJECT_0 + len(handles):
                phandle = handles[val - WAIT_OBJECT_0]
                handles.remove(phandle)
                transport = self.phandleToTransport[phandle]
                phandleKey = self.phandleToPhandleKey[phandle]
                self.reactor.callFromThread(self.processEnded, phandle, phandleKey)
            elif val == WAIT_OBJECT_0 + len(handles):
                status, msg = win32gui.PeekMessage(theWindow,
                                                   0,
                                                   0,
                                                   win32con.PM_REMOVE)
                while status != 0:
                    if msg[1] == WM_NEW_PHANDLE:
                        phandleKey = msg[2]
                        handles.append(self.needWaiting[phandleKey])
                    elif msg[1] == WM_CLOSE_THREAD:
                        return
                    else:
                        pass
                    status, msg = win32gui.PeekMessage(
                            theWindow,
                            0,
                            0,
                            win32con.PM_REMOVE)
            else:
                raise Exception("MsgWaitForMultipleObjects returned unknown value: %s" % str(val))
    def processEnded(self, processHandle, processHandleKey):
        wfmoThread = self.phandleKeyToThreadHandle[processHandleKey]
        processTransport = self.phandleToTransport[processHandle]
        self.threadToNumEnded[wfmoThread] += 1
        self.threadToNumProcessHandles[wfmoThread] -= 1
        if self.threadToNumProcessHandles[wfmoThread] == 62:
            self.availableThreads.append(wfmoThread)
            self.usedThreads.remove(wfmoThread)
        elif self.threadToNumProcessHandles[wfmoThread] == 0:
            self.availableThreads.remove(wfmoThread)
            if not self.threadToMsgWindowCreated[wfmoThread]:
                val = WaitForSingleObject(self.threadToMsgWindowCreationEvent[wfmoThread], INFINITE)
                if val != WAIT_OBJECT_0:
                    raise RuntimeError("WaitForSingleObject returned %d.  It should only return %d" % (val, WAIT_OBJECT_0))
            win32api.PostMessage(
                    self.threadToMsgWindow[wfmoThread], # thread id
                    WM_CLOSE_THREAD, # message 
                    0, # wParam
                    0 # lParam
                    )
            del self.threadToNumProcessHandles[wfmoThread]
            del self.threadToMsgWindowCreated[wfmoThread]
        del self.needWaiting[processHandleKey]
        del self.phandleToTransport[processHandle]
        processTransport.processEnded()
