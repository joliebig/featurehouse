import pythoncom
import os, sys
from win32com.mapi import mapi, mapiutil
from win32com.mapi.mapitags import *
MSGFLAG_READ = 0x1
CLEAR_READ_FLAG = 0x00000004
CLEAR_RN_PENDING = 0x00000020
CLEAR_NRN_PENDING = 0x00000040
SUPPRESS_RECEIPT = 0x1
import mapi_driver
def SetReadState(driver, mapi_folder, subject, unread):
    hr, data = mapi_folder.GetProps( (PR_DISPLAY_NAME_A,), 0)
    name = data[0][1]
    num = 0
    for item in driver.GetItemsWithValue(mapi_folder, PR_SUBJECT_A, subject):
        flags_base = mapi.MAPI_DEFERRED_ERRORS | SUPPRESS_RECEIPT
        if unread:
            item.SetReadFlag(mapi.MAPI_DEFERRED_ERRORS|CLEAR_READ_FLAG)
        else:
            item.SetReadFlag(flags_base)
        num += 1
        hr, props = item.GetProps((PR_MESSAGE_FLAGS,), 0)
        ((tag, val), ) = props
        if val & MSGFLAG_READ == unread:
            print("MAPI SetReadState appears to have failed to change the message state")
            print("Requested set to unread=%s but the MAPI field after was %r" % \
                    (unread, val))
    print("Processed", num, "items")
def usage(driver):
    folder_doc = driver.GetFolderNameDoc()
    msg = """\
Usage: %s [-u] subject of the message
-f - Search for the message in the specified folder (default = Inbox)
-u - Mark as unread
Marks as read (or unread) all messages that match the subject.  Subject
matching is substring and ignore-case.
%s
Use the -n option to see all top-level folder names from all stores.""" \
    % (os.path.basename(sys.argv[0]),folder_doc)
    print(msg)
def main():
    driver = mapi_driver.MAPIDriver()
    import getopt
    try:
        opts, args = getopt.getopt(sys.argv[1:], "u")
    except getopt.error as e:
        print(e)
        print()
        usage(driver)
        sys.exit(1)
    folder_name = ""
    unread = False
    for opt, opt_val in opts:
        if opt == "-u":
            unread = True
        else:
            print("Invalid arg")
            return
    if not folder_name:
        folder_name = "Inbox" 
    subject = " ".join(args)
    if not subject:
        print("You must specify a subject")
        print()
        usage(driver)
        sys.exit(1)
    try:
        folder = driver.FindFolder(folder_name)
    except ValueError as details:
        print(details)
        sys.exit(1)
    SetReadState(driver, folder, subject, unread)
if __name__=='__main__':
    main()
