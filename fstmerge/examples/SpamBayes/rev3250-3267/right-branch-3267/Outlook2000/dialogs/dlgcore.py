import win32gui, win32api, win32con
import commctrl
import struct, array
from .dlgutils import *
class TooltipManager:
    def __init__(self, dialog):
        self.dialog = dialog
        self.hwnd_tooltip = None
        self.tooltip_tools = {}
    def HideTooltip(self):
        if self.hwnd_tooltip is not None:
            win32gui.SendMessage(self.hwnd_tooltip, commctrl.TTM_TRACKACTIVATE, 0, 0)
    def ShowTooltipForControl(self, control_id, text):
        hwnd_dialog = self.dialog.hwnd
        self.HideTooltip()
        if self.hwnd_tooltip is None:
            TTS_BALLOON = 0x40
            self.hwnd_tooltip = win32gui.CreateWindowEx(0, "tooltips_class32", None,
                                    win32con.WS_POPUP | TTS_BALLOON,
                                    win32con.CW_USEDEFAULT, win32con.CW_USEDEFAULT,
                                    win32con.CW_USEDEFAULT, win32con.CW_USEDEFAULT,
                                    hwnd_dialog, 0, 0, None)
            win32gui.SendMessage(self.hwnd_tooltip,
                                 commctrl.TTM_SETMAXTIPWIDTH,
                                 0, 300)
        format = "iiiiiiiiiii"
        tt_size = struct.calcsize(format)
        buffer = array.array("c", text + "\0")
        text_address, size = buffer.buffer_info()
        uID = control_id
        flags = commctrl.TTF_TRACK | commctrl.TTF_ABSOLUTE
        data = struct.pack(format, tt_size, flags, hwnd_dialog, uID, 0,0,0,0, 0, text_address, 0)
        if control_id not in self.tooltip_tools:
            win32gui.SendMessage(self.hwnd_tooltip,
                                 commctrl.TTM_ADDTOOL,
                                 0, data)
            self.tooltip_tools[control_id] = 1
        control = win32gui.GetDlgItem(hwnd_dialog, control_id)
        child_rect = win32gui.GetWindowRect(control)
        xOff = yOff = 15 
        win32gui.SendMessage(self.hwnd_tooltip,
                             commctrl.TTM_TRACKPOSITION,
                             0,
                             MAKELONG(child_rect[0]+xOff, child_rect[1]+yOff))
        win32gui.SendMessage(self.hwnd_tooltip,
                             commctrl.TTM_TRACKACTIVATE,
                             1,data)
class Dialog:
    def __init__(self, parent, parser, idd):
        win32gui.InitCommonControls()
        self.hinst = win32api.GetModuleHandle(None)
        self.parent = parent
        self.dialog_parser = parser
        self.template = parser.dialogs[idd]
    def _GetIDName(self, cid):
        return self.dialog_parser.names.get(cid, str(cid))
    def CreateWindow(self):
        return self._DoCreate(win32gui.CreateDialogIndirect)
    def DoModal(self):
        return self._DoCreate(win32gui.DialogBoxIndirect)
    def GetMessageMap(self):
        ret = {
            win32con.WM_COMMAND: self.OnCommand,
            win32con.WM_NOTIFY: self.OnNotify,
            win32con.WM_INITDIALOG: self.OnInitDialog,
            win32con.WM_CLOSE: self.OnClose,
            win32con.WM_DESTROY: self.OnDestroy,
            win32con.WM_RBUTTONUP: self.OnRButtonUp,
        }
        return ret
    def DoInitialPosition(self):
        desktop = win32gui.GetDesktopWindow()
        l,t,r,b = win32gui.GetWindowRect(self.hwnd)
        w = r-l
        h = b-t
        dt_l, dt_t, dt_r, dt_b = win32gui.GetWindowRect(desktop)
        centre_x, centre_y = win32gui.ClientToScreen( desktop, ( (dt_r-dt_l)/2, (dt_b-dt_t)/2) )
        win32gui.MoveWindow(self.hwnd, centre_x-(w/2), centre_y-(h/2), w, h, 0)
    def OnInitDialog(self, hwnd, msg, wparam, lparam):
        self.hwnd = hwnd
        self.DoInitialPosition()
    def OnCommand(self, hwnd, msg, wparam, lparam):
        pass
    def OnNotify(self, hwnd, msg, wparam, lparam):
        pass
    def OnClose(self, hwnd, msg, wparam, lparam):
        pass
    def OnDestroy(self, hwnd, msg, wparam, lparam):
        pass
    def OnRButtonUp(self, hwnd, msg, wparam, lparam):
        pass
    def _DoCreate(self, fn):
        message_map = self.GetMessageMap()
        return win32gui.DialogBoxIndirect(self.hinst, self.template, self.parent, message_map)
    def GetDlgItem(self, id):
        if type(id)==type(''):
            id = self.dialog_parser.ids[id]
        return win32gui.GetDlgItem(self.hwnd, id)
    def SetDlgItemText(self, id, text):
        hchild = self.GetDlgItem(id)
        win32gui.SendMessage(hchild, win32con.WM_SETTEXT, 0, text)
class TooltipDialog(Dialog):
    def __init__(self, parent, parser, idd):
        Dialog.__init__(self, parent, parser, idd)
        self.tt = TooltipManager(self)
    def GetMessageMap(self):
        ret = Dialog.GetMessageMap(self)
        ret.update( {
            win32con.WM_HELP: self.OnHelp,
            win32con.WM_LBUTTONDOWN: self.OnLButtonDown,
            win32con.WM_ACTIVATE: self.OnActivate,
        })
        return ret
    def OnLButtonDown(self, hwnd, msg, wparam, lparam):
        self.tt.HideTooltip()
    def OnActivate(self, hwnd, msg, wparam, lparam):
        self.tt.HideTooltip()
    def OnDestroy(self, hwnd, msg, wparam, lparam):
        self.tt.HideTooltip()
    def OnHelp(self, hwnd, msg, wparam, lparam):
        format = "iiiiiii"
        buf = win32gui.PyMakeBuffer(struct.calcsize(format), lparam)
        cbSize, iContextType, iCtrlId, hItemHandle, dwContextID, x, y = \
                struct.unpack(format, buf)
        tt_text = self.GetPopupHelpText(iCtrlId)
        if tt_text:
            self.tt.ShowTooltipForControl(iCtrlId, tt_text)
        else:
            self.tt.HideTooltip()
        return 1
    def GetPopupHelpText(self, control_id):
        return None
class ProcessorDialog(TooltipDialog):
    def __init__(self, parent, manager, config, idd, option_handlers):
        TooltipDialog.__init__(self, parent, manager.dialog_parser, idd)
        parser = manager.dialog_parser
        self.manager = manager
        self.config = config
        self.command_processors = {}
        self.processor_message_map = {} 
        self.all_processors = []
        for data in option_handlers:
            klass = data[0]
            id_names = data[1]
            rest = data[2:]
            ids = id_names.split()
            int_ids = [ parser.ids[id] for id in ids]
            instance = klass(self,int_ids, *rest)
            self.all_processors.append(instance)
            for int_id in int_ids:
                self.command_processors[int_id] = instance
            for message in instance.GetMessages():
                existing = self.processor_message_map.setdefault(message, [])
                existing.append(instance)
    def GetMessageMap(self):
        ret = TooltipDialog.GetMessageMap(self)
        for key in list(self.processor_message_map.keys()):
            if key in ret:
                print("*** WARNING: Overwriting message!!!")
            ret[key] = self.OnCommandProcessorMessage
        return ret
    def OnInitDialog(self, hwnd, msg, wparam, lparam):
        TooltipDialog.OnInitDialog(self, hwnd, msg, wparam, lparam)
        if __debug__: 
            for int_id in self.command_processors:
                try:
                    self.GetDlgItem(int_id)
                except win32gui.error:
                    print("ERROR: Dialog item %s refers to an invalid control" % \
                          self._GetIDName(int_id))
        self.LoadAllControls()
    def GetPopupHelpText(self, iCtrlId):
        cp = self.command_processors.get(iCtrlId)
        tt_text = None
        if cp is not None:
            return cp.GetPopupHelpText(iCtrlId)
        print("Can not get command processor for", self._GetIDName(iCtrlId))
        return None
    def OnRButtonUp(self, hwnd, msg, wparam, lparam):
        for cp in list(self.command_processors.values()):
            cp.OnRButtonUp(wparam,lparam)
    def OnCommandProcessorMessage(self, hwnd, msg, wparam, lparam):
        for p in self.processor_message_map[msg]:
            p.OnMessage(msg, wparam, lparam)
    def OnOptionChanged(self, changed_by, option):
        for p in self.all_processors:
            if p is not changed_by:
                p.OnOptionChanged(option)
    def OnDestroy(self, hwnd, msg, wparam, lparam):
        for p in self.all_processors:
            p.Term()
        TooltipDialog.OnDestroy(self, hwnd, msg, wparam, lparam)
        self.command_processors = None
        self.all_processors = None
        self.processor_message_map = None
    def LoadAllControls(self):
        for p in self.all_processors:
            p.Init()
    def ApplyHandlingOptionValueError(self, func, *args):
        try:
            return func(*args)
        except ValueError as why:
            mb_flags = win32con.MB_ICONEXCLAMATION | win32con.MB_OK
            win32gui.MessageBox(self.hwnd, str(why), "SpamBayes", mb_flags)
            return False
    def SaveAllControls(self):
        for p in self.all_processors:
            if not self.ApplyHandlingOptionValueError(p.Done):
                win32gui.SetFocus(p.GetControl())
                return False
        return True
    def OnClose(self, hwnd, msg, wparam, lparam):
        if TooltipDialog.OnClose(self, hwnd, msg, wparam, lparam):
            return 1
        if not self.SaveAllControls():
            return 1
        win32gui.EndDialog(hwnd, 0)
    def OnNotify(self, hwnd, msg, wparam, lparam):
        TooltipDialog.OnNotify(self, hwnd, msg, wparam, lparam)
        format = "iii"
        buf = win32gui.PyMakeBuffer(struct.calcsize(format), lparam)
        hwndFrom, idFrom, code = struct.unpack(format, buf)
        code += 0x4f0000 
        handler = self.command_processors.get(idFrom)
        if handler is None:
            print("Ignoring OnNotify for", self._GetIDName(idFrom))
            return
        return handler.OnNotify( (hwndFrom, idFrom, code), wparam, lparam)
    def OnCommand(self, hwnd, msg, wparam, lparam):
        TooltipDialog.OnCommand(self, hwnd, msg, wparam, lparam)
        id = win32api.LOWORD(wparam)
        if self.command_processors is None:
            print("Ignoring OnCommand for", self._GetIDName(id))
            return
        else:
            handler = self.command_processors.get(id)
            if handler is None:
                print("Ignoring OnCommand for", self._GetIDName(id))
                return
        self.ApplyHandlingOptionValueError(handler.OnCommand, wparam, lparam)
class ProcessorPage(ProcessorDialog):
    def __init__(self, parent, manager, config, idd, option_handlers, yoffset):
        ProcessorDialog.__init__(self, parent, manager, config, idd,option_handlers)
        self.yoffset = yoffset
    def DoInitialPosition(self):
        win32gui.SetWindowPos(self.hwnd, win32con.HWND_TOP, 1, self.yoffset, 0, 0, win32con.SWP_NOSIZE)
    def CreateWindow(self):
        message_map = self.GetMessageMap()
        self.template[0][2] = self.template[0][2] & ~(win32con.DS_MODALFRAME|win32con.WS_POPUP|win32con.WS_OVERLAPPED|win32con.WS_CAPTION)
        self.template[0][2] = self.template[0][2] | win32con.WS_CHILD
        return win32gui.CreateDialogIndirect(self.hinst, self.template, self.parent, message_map)
