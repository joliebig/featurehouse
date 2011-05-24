"""dump one or more items as an 'email object' to stdout."""
import sys, os
import optparse
from win32com.mapi import mapi, mapiutil
from win32com.mapi.mapitags import *
import win32clipboard
try:
    from manager import BayesManager
except ImportError:
    if hasattr(sys, "frozen"):
        raise
    sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
    from manager import BayesManager
import mapi_driver
from cStringIO import StringIO
def Dump(driver, manager, mapi_folder, subject, stream=None):
    for item in driver.GetItemsWithValue(mapi_folder, PR_SUBJECT_A, subject):
        hr, props = item.GetProps((PR_ENTRYID,PR_STORE_ENTRYID), 0)
        (tag, eid), (tag, store_eid) = props
        eid = mapi.HexFromBin(eid)
        store_eid = mapi.HexFromBin(store_eid)
        print >> stream, "Dumping message with ID %s/%s" % (store_eid, eid)
        msm = manager.message_store.GetMessage((store_eid, eid))
        ob = msm.GetEmailPackageObject()
        print >> stream, ob.as_string()
        print >> stream
def main():
    driver = mapi_driver.MAPIDriver()
    parser = optparse.OptionParser("%prog [options] [path ...]",
                                   description=__doc__)
    parser.add_option("-q", "--quiet",
                      action="store_true", dest="quiet", default=False,
                      help="don't print status messages to stdout")
    parser.add_option("-f", "--folder",
                      action="store", default="Inbox",
                      help="folder to search")
    parser.add_option("-c", "--clipboard",
                      action="store",
                      help="write results to the clipboard")
    options, args = parser.parse_args()
    subject = " ".join(args)
    try:
        folder = driver.FindFolder(options.folder)
    except ValueError, details:
        parser.error(details)
    stream = None
    if options.clipboard:
        stream = StringIO()
    Dump(driver, BayesManager(), folder, subject, stream)
    if options.clipboard:
        win32clipboard.OpenClipboard()
        win32clipboard.EmptyClipboard()
        win32clipboard.SetClipboardText(stream.getvalue())
        print "Output successfuly written to the Windows clipboard"
if __name__=='__main__':
    main()
