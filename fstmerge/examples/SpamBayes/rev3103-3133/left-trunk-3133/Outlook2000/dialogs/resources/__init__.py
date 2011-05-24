def GetImageParamsFromBitmapID(rc_parser, bmpid):
    import os, sys
    import win32gui, win32con, win32api
    if type(bmpid)==type(0):
        bmpid = rc_parser.names[bmpid]
    int_bmpid = rc_parser.ids[bmpid]
    filename = rc_parser.bitmaps[bmpid]
    if hasattr(sys, "frozen"):
        if sys.frozen=="dll":
            hmod = sys.frozendllhandle
        else:
            hmod = win32api.GetModuleHandle(None)
        return hmod, int_bmpid, 0
    else:
        if not os.path.isabs(filename):
            filename = os.path.join( os.path.dirname( __file__ ), filename)
        return 0, filename, win32con.LR_LOADFROMFILE
    assert 0, "not reached"
