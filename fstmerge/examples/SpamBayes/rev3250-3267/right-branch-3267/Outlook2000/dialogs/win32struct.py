import struct, array
import win32gui
class cStruct(object):
    def __init__(self, sd):
        self.sd = list(sd)
        self.nlst = [i[0] for i in sd]
        self.fs = "".join([i[1] for i in sd])
        t = [i[2] for i in sd]
        self.data = struct.pack(self.fs, *t)
    def __setattr__(self, name, v):
        if name in ['sd', 'nlst', 'fs', 'data', 'ptr'] or name not in self.nlst:
            object.__setattr__(self, name, v)
            return
        t = list(struct.unpack(self.fs, self.data))
        i = self.nlst.index(name)
        if i > -1:
            t[i] = v
        self.data = struct.pack(self.fs, *t)
    def __getattr__(self, name):
        t = struct.unpack(self.fs, self.data)
        i = self.nlst.index(name)
        if i > -1:
            return t[i]
        else:
            raise AttributeError
    def __str__(self):
        return self.data
    def dump(self): # use this to see the data
        t = struct.unpack(self.fs, self.data)
        ii = 0
        for i in self.nlst:
            print(i, "=", t[ii])
            ii += 1
        print("fs =", self.fs)
        return
class OPENFILENAME(cStruct):
    _struct_def = ( \
        ('lStructSize', 'L', 0),    # size of struct (filled in by __init__)
        ('hwndOwner', 'P', 0),
        ('hInstance', 'P', 0),
        ('lpstrFilter', 'P', 0),    # File type filter
        ('lpstrCustomFilter', 'P', 0), 
        ('nMaxCustFilter', 'L', 0),
        ('nFilterIndex', 'L', 0),
        ('lpstrFile', 'P', 0),      # Initial filename and filename buffer
        ('nMaxFile', 'L', 0),       # Size of filename string (should be >= 256)
        ('lpstrFileTitle', 'P', 0),  # (optional) base name receiving buffer
        ('nMaxFileTitle', 'L', 0),  # max size of above
        ('lpstrInitialDir', 'P', 0), # (optional) initial directory
        ('lpstrTitle', 'P', 0),     # Title of dialog
        ('Flags', 'L', 0),
        ('nFileOffset', 'H', 0),
        ('nFileExtension', 'H', 0),
        ('lpstrDefExt', 'P', 0),    # default extension
        ('lCustData', 'l', 0),
        ('lpfnHook', 'P', 0),
        ('lpTemplateName', 'P', 0)
    )
    def __init__(self, max_filename_len):
        cStruct.__init__(self, self._struct_def)
        self.lStructSize = struct.calcsize(self.fs)
        self.fn_buf = array.array("c", '\0'*max_filename_len)
        self.fn_buf_addr, self.fn_buf_len = self.fn_buf.buffer_info()
        self.lpstrFile = self.fn_buf_addr
        self.nMaxFile = self.fn_buf_len
    def setFilename(self, filename):
        win32gui.PySetString(self.fn_buf_addr, filename, self.fn_buf_len - 1)
    def getFilename(self):
        return win32gui.PyGetString(self.fn_buf_addr)
    def setTitle(self, title):
        if isinstance(title, str):
            title = title.encode("mbcs")
        self.title_buf = array.array("c", title+'\0')
        self.lpstrTitle = self.title_buf.buffer_info()[0]
    def setInitialDir(self, initialDir):
        if isinstance(initialDir, str):
            initialDir = initialDir.encode("mbcs")
        self.initialDir_buf = array.array("c", initialDir+'\0')
        self.lpstrInitialDir = self.initialDir_buf.buffer_info()[0]
    def setFilter(self, fileFilter):
        if isinstance(fileFilter, str):
            fileFilter = fileFilter.encode("mbcs")
        fileFilter = fileFilter.replace('|', '\0') + '\0'
        self.fileFilter_buf = array.array("c", fileFilter+'\0')
        self.lpstrFilter = self.fileFilter_buf.buffer_info()[0]
