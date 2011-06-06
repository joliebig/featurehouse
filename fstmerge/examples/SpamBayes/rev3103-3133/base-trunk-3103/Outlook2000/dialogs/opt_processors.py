import win32gui, win32api, win32con
import commctrl
import struct, array
from dlgutils import *
import processors
verbose = 0 # set to 1 to see option values fetched and set.
class OptionControlProcessor(processors.ControlProcessor):
    def __init__(self, window, control_ids, option):
        processors.ControlProcessor.__init__(self, window, control_ids)
        if option:
            sect_name, option_name = option.split(".")
            self.option = window.config.get_option(sect_name, option_name)
        else:
            self.option = None
    def GetPopupHelpText(self, idFrom):
        doc = " ".join(self.option.doc().split())
        if self.option.default_value:
            doc += " (the default value is %s)" % self.option.default_value
        return doc
    def Init(self):
        self.UpdateControl_FromValue()
    def Done(self):
        self.UpdateValue_FromControl()
        return True
    def NotifyOptionChanged(self, option = None):
        if option is None:
            option = self.option
        self.window.OnOptionChanged(self, option)
    def SetOptionValue(self, value, option = None):
        if option is None:
            option = self.option
        if verbose:
            print "Setting option '%s' (%s) -> %s" % \
                  (option.display_name(), option.name, value)
        option.set(value)
        self.NotifyOptionChanged(option)
    def GetOptionValue(self, option = None):
        if option is None:
            option = self.option
        ret = option.get()
        if verbose:
            print "Got option '%s' (%s) -> %s" % \
                  (option.display_name(), option.name, ret)
        return ret
    def UpdateControl_FromValue(self):
        raise NotImplementedError
    def UpdateValue_FromControl(self):
        raise NotImplementedError
class BoolButtonProcessor(OptionControlProcessor):
    def __init__(self, window, control_ids, option, disable_when_false_ids=""):
        OptionControlProcessor.__init__(self, window, control_ids, option)
        self.disable_ids = [window.manager.dialog_parser.ids[id]
                            for id in disable_when_false_ids.split()]
    def OnCommand(self, wparam, lparam):
        code = win32api.HIWORD(wparam)
        if code == win32con.BN_CLICKED:
            self.UpdateValue_FromControl()
    def UpdateEnabledStates(self, enabled):
        for other in self.disable_ids:
            win32gui.EnableWindow(self.GetControl(other), enabled)
    def UpdateControl_FromValue(self):
        value = self.GetOptionValue()
        win32gui.SendMessage(self.GetControl(), win32con.BM_SETCHECK, value)
        for other in self.other_ids:
            win32gui.SendMessage(self.GetControl(other), win32con.BM_SETCHECK, not value)
        self.UpdateEnabledStates(value)
    def UpdateValue_FromControl(self):
        check = win32gui.SendMessage(self.GetControl(), win32con.BM_GETCHECK)
        check = not not check # force bool!
        self.SetOptionValue(check)
        self.UpdateEnabledStates(check)
class RadioButtonProcessor(OptionControlProcessor):
    def OnCommand(self, wparam, lparam):
        code = win32api.HIWORD(wparam)
        if code == win32con.BN_CLICKED:
            self.UpdateValue_FromControl()
    def UpdateControl_FromValue(self):
        value = self.GetOptionValue()
        i = 0
        first = chwnd = self.GetControl()
        while chwnd:
            if i==value:
                win32gui.SendMessage(chwnd, win32con.BM_SETCHECK, 1)
                break
            chwnd = win32gui.GetNextDlgGroupItem(self.window.hwnd,
                                               chwnd,
                                               False)
            assert chwnd!=first, "Back where I started!"
            i += 1
        else:
            assert 0, "Could not find control for value %s" % value
    def UpdateValue_FromControl(self):
        all_ids = [self.control_id] + self.other_ids
        chwnd = self.GetControl()
        i = 0
        while chwnd:
            checked = win32gui.SendMessage(chwnd, win32con.BM_GETCHECK)
            if checked:
                self.SetOptionValue(i)
                break
            chwnd = win32gui.GetNextDlgGroupItem(self.window.hwnd, chwnd, False)
            i += 1
        else:
            assert 0, "Couldn't find a checked button"
class ComboProcessor(OptionControlProcessor):
    def __init__(self, window, control_ids, option,text=None):
        OptionControlProcessor.__init__(self, window, control_ids, option)
        if text:
            temp = text.split(",")
            self.option_to_text = zip(self.option.valid_input(), temp)
            self.text_to_option = dict(zip(temp, self.option.valid_input()))
        else:
            self.option_to_text = zip(self.option.valid_input(),self.option.valid_input())
            self.text_to_option = dict(self.option_to_text)
    def OnCommand(self, wparam, lparam):
        code = win32api.HIWORD(wparam)
        if code == win32con.CBN_SELCHANGE:
            self.UpdateValue_FromControl()
    def UpdateControl_FromValue(self):
        combo = self.GetControl()
        index = sel_index = 0
        value = self.GetOptionValue()
        for opt,text in self.option_to_text:
            win32gui.SendMessage(combo, win32con.CB_ADDSTRING, 0, text)
            if value.startswith(opt):
                sel_index = index
            index += 1
        win32gui.SendMessage(combo, win32con.CB_SETCURSEL, sel_index, 0)
    def UpdateValue_FromControl(self):
        combo = self.GetControl()
        sel = win32gui.SendMessage(combo, win32con.CB_GETCURSEL)
        len = win32gui.SendMessage(combo, win32con.CB_GETLBTEXTLEN, sel)
        buffer = array.array("c", "\0" * (len + 1))
        win32gui.SendMessage(combo, win32con.CB_GETLBTEXT, sel, buffer)
        text = buffer.tostring()[:-1]
        self.SetOptionValue(self.text_to_option[text])
class EditNumberProcessor(OptionControlProcessor):
    def __init__(self, window, control_ids, option, min_val=0, max_val=100,
                 ticks=100, max_edit_val=100):
        self.slider_id = control_ids and control_ids[1]
        self.min_val = min_val
        self.max_val = max_val
        self.max_edit_val = max_edit_val
        self.ticks = ticks
        OptionControlProcessor.__init__(self, window, control_ids, option)
    def GetPopupHelpText(self, id):
        if id == self.slider_id:
            return "As you drag this slider, the value to the right will " \
                   "automatically adjust"
        return OptionControlProcessor.GetPopupHelpText(self, id)
    def GetMessages(self):
        return [win32con.WM_HSCROLL]
    def OnMessage(self, msg, wparam, lparam):
        slider = self.GetControl(self.slider_id)
        if slider == lparam:
            slider_pos = win32gui.SendMessage(slider, commctrl.TBM_GETPOS, 0, 0)
            slider_pos = float(slider_pos) * self.max_val / self.ticks
            str_val = str(slider_pos)
            edit = self.GetControl()
            win32gui.SendMessage(edit, win32con.WM_SETTEXT, 0, str_val)
    def OnCommand(self, wparam, lparam):
        code = win32api.HIWORD(wparam)
        if code==win32con.EN_CHANGE:
            try:
                self.UpdateValue_FromControl()
                self.UpdateSlider_FromEdit()
            except ValueError:
                pass
    def Init(self):
        OptionControlProcessor.Init(self)
        if self.slider_id:
            self.InitSlider()
    def InitSlider(self):
        slider = self.GetControl(self.slider_id)
        assert self.min_val == 0, "sue me"
        win32gui.SendMessage(slider, commctrl.TBM_SETRANGE, 0, MAKELONG(0, self.ticks))
        win32gui.SendMessage(slider, commctrl.TBM_SETLINESIZE, 0, 1)
        win32gui.SendMessage(slider, commctrl.TBM_SETPAGESIZE, 0, self.ticks/20)
        win32gui.SendMessage(slider, commctrl.TBM_SETTICFREQ, self.ticks/10, 0)
    def UpdateControl_FromValue(self):
        win32gui.SendMessage(self.GetControl(), win32con.WM_SETTEXT, 0,
                             str(self.GetOptionValue()))
        self.UpdateSlider_FromEdit()
    def UpdateSlider_FromEdit(self):
        slider = self.GetControl(self.slider_id)
        try:
            val = float(self.GetOptionValue())
            val *= float(self.ticks) / self.max_val
            val = int(val)
        except ValueError:
            return
        win32gui.SendMessage(slider, commctrl.TBM_SETPOS, 1, val)
    def UpdateValue_FromControl(self):
        buf_size = 100
        buf = win32gui.PyMakeBuffer(buf_size)
        nchars = win32gui.SendMessage(self.GetControl(), win32con.WM_GETTEXT,
                                      buf_size, buf)
        str_val = buf[:nchars]
        val = float(str_val)
        if val < self.min_val or val > self.max_edit_val:
            raise ValueError, "Value must be between %d and %d" % (self.min_val, self.max_val)
        self.SetOptionValue(val)
class FilenameProcessor(OptionControlProcessor):
    def __init__(self, window, control_ids, option, file_filter="All Files|*.*"):
        self.button_id = control_ids[1]
        self.file_filter = file_filter
        OptionControlProcessor.__init__(self, window, control_ids, option)
    def GetPopupHelpText(self, idFrom):
        if idFrom == self.button_id:
            return "Displays a dialog from which you can select a file."
        return OptionControlProcessor.GetPopupHelpText(self, id)
    def DoBrowse(self):
        from win32struct import OPENFILENAME
        ofn = OPENFILENAME(512)
        ofn.hwndOwner = self.window.hwnd
        ofn.setFilter(self.file_filter)
        ofn.setTitle(_("Browse for file"))
        def_filename = self.GetOptionValue()
        if (len(def_filename) > 0):
            from os.path import basename
            ofn.setInitialDir(basename(def_filename))
        ofn.setFilename(def_filename)
        ofn.Flags = win32con.OFN_FILEMUSTEXIST
        retval = win32gui.GetOpenFileName(str(ofn))
        if (retval == win32con.IDOK):
            self.SetOptionValue(ofn.getFilename())
            self.UpdateControl_FromValue()
            return True
        return False
    def OnCommand(self, wparam, lparam):
        id = win32api.LOWORD(wparam)
        code = win32api.HIWORD(wparam)
        if id == self.button_id:
            self.DoBrowse()
        elif code==win32con.EN_CHANGE:
            self.UpdateValue_FromControl()
    def UpdateControl_FromValue(self):
        win32gui.SendMessage(self.GetControl(), win32con.WM_SETTEXT, 0,
                             self.GetOptionValue())
    def UpdateValue_FromControl(self):
        buf_size = 256
        buf = win32gui.PyMakeBuffer(buf_size)
        nchars = win32gui.SendMessage(self.GetControl(), win32con.WM_GETTEXT,
                                      buf_size, buf)
        str_val = buf[:nchars]
        self.SetOptionValue(str_val)
class FolderIDProcessor(OptionControlProcessor):
    def __init__(self, window, control_ids,
                 option, option_include_sub = None,
                 use_fqn = False, name_joiner = "; "):
        self.button_id = control_ids[1]
        self.use_fqn = use_fqn
        self.name_joiner = name_joiner
        if option_include_sub:
            incl_sub_sect_name, incl_sub_option_name = \
                                option_include_sub.split(".")
            self.option_include_sub = \
                            window.config.get_option(incl_sub_sect_name,
                                                      incl_sub_option_name)
        else:
            self.option_include_sub = None
        OptionControlProcessor.__init__(self, window, control_ids, option)
    def DoBrowse(self):
        mgr = self.window.manager
        is_multi = self.option.multiple_values_allowed()
        if is_multi:
            ids = self.GetOptionValue()
        else:
            ids = [self.GetOptionValue()]
        from dialogs import FolderSelector
        if self.option_include_sub:
            cb_state = self.option_include_sub.get()
        else:
            cb_state = None # don't show it.
        d = FolderSelector.FolderSelector(self.window.hwnd,
                                            mgr,
                                            ids,
                                            single_select=not is_multi,
                                            checkbox_state=cb_state)
        if d.DoModal() == win32con.IDOK:
            ids, include_sub = d.GetSelectedIDs()
            if is_multi:
                self.SetOptionValue(ids)
            else:
                self.SetOptionValue(ids[0])
            if self.option_include_sub:
                self.SetOptionValue(include_sub, self.option_include_sub)
            self.UpdateControl_FromValue()
            return True
        return False
    def OnCommand(self, wparam, lparam):
        id = win32api.LOWORD(wparam)
        if id == self.button_id:
            self.DoBrowse()
    def GetPopupHelpText(self, idFrom):
        if idFrom == self.button_id:
            return "Displays a list from which you can select folders."
        return OptionControlProcessor.GetPopupHelpText(self, idFrom)
    def UpdateControl_FromValue(self):
        mgr = self.window.manager
        if self.option.multiple_values_allowed():
            ids = self.GetOptionValue()
        else:
            ids = [self.GetOptionValue()]
        names = []
        for eid in ids:
            if eid is not None:
                try:
                    folder = mgr.message_store.GetFolder(eid)
                    if self.use_fqn:
                        name = folder.GetFQName()
                    else:
                        name = folder.name
                except mgr.message_store.MsgStoreException:
                    name = "<unknown folder>"
                names.append(name)
        win32gui.SetWindowText(self.GetControl(), self.name_joiner.join(names))
    def UpdateValue_FromControl(self):
        pass
