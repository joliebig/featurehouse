import os
import pythoncom
from win32com.mapi import mapi, mapiutil
from win32com.mapi.mapitags import *
from win32com.client import Dispatch
class MAPIDriver:
    def __init__(self, read_only = False):
        old_cwd = os.getcwd()
        mapi.MAPIInitialize(None)
        logonFlags = (mapi.MAPI_NO_MAIL |
                      mapi.MAPI_EXTENDED |
                      mapi.MAPI_USE_DEFAULT)
        self.session = mapi.MAPILogonEx(0, None, None, logonFlags)
        if read_only:
            self.mapi_flags = mapi.MAPI_DEFERRED_ERRORS
        else:
            self.mapi_flags = mapi.MAPI_DEFERRED_ERRORS | mapi.MAPI_BEST_ACCESS
        self.outlook = None
        os.chdir(old_cwd)
    def _GetMAPIFlags(self, mapi_flags = None):
        if mapi_flags is None:
            mapi_flags = self.mapi_flags
        return mapi_flags
    def GetOutlookFolder(self, item):
        if self.outlook is None:
            self.outlook = Dispatch("Outlook.Application")
        hr, props = item.GetProps((PR_ENTRYID,PR_STORE_ENTRYID), 0)
        (tag, eid), (tag, store_eid) = props
        eid = mapi.HexFromBin(eid)
        store_eid = mapi.HexFromBin(store_eid)
        return self.outlook.Session.GetFolderFromID(eid, store_eid)
    def GetMessageStores(self):
        tab = self.session.GetMsgStoresTable(0)
        rows = mapi.HrQueryAllRows(tab,
                                   (PR_ENTRYID, PR_DISPLAY_NAME_A, PR_DEFAULT_STORE),   # columns to retrieve
                                   None,     # all rows
                                   None,            # any sort order is fine
                                   0)               # any # of results is fine
        for row in rows:
            (eid_tag, eid), (name_tag, name), (def_store_tag, def_store) = row
            try:
                store = self.session.OpenMsgStore(
                                    0,      # no parent window
                                    eid,    # msg store to open
                                    None,   # IID; accept default IMsgStore
                                    mapi.MDB_WRITE |
                                        mapi.MDB_NO_MAIL |
                                        mapi.MAPI_DEFERRED_ERRORS)
                yield store, name, def_store
            except pythoncom.com_error as details:
                hr, msg, exc, arg_err = details
                if hr== mapi.MAPI_E_FAILONEPROVIDER:
                    pass
                else:
                    print("Error opening message store", details, "- ignoring")
    def _FindSubfolder(self, store, folder, find_name):
        find_name = find_name.lower()
        table = folder.GetHierarchyTable(0)
        rows = mapi.HrQueryAllRows(table, (PR_ENTRYID, PR_DISPLAY_NAME_A), None, None, 0)
        for (eid_tag, eid), (name_tag, name), in rows:
            if name.lower() == find_name:
                return store.OpenEntry(eid, None, mapi.MAPI_DEFERRED_ERRORS)
        return None
    def FindFolder(self, name):
        assert name
        names = [n.lower() for n in name.split("\\")]
        if names[0]:
            store_name = None
            for store, name, is_default in self.GetMessageStores():
                if is_default:
                    store_name = name.lower()
                    break
            if store_name is None:
                raise RuntimeError("Can't find a default message store")
            folder_names = names
        else:
            store_name = names[1]
            folder_names = names[2:]
        for store, name, is_default in self.GetMessageStores():
            if name.lower() == store_name:
                folder_store = store
                break
        else:
            raise ValueError("The store '%s' can not be located" % (store_name,))
        hr, data = store.GetProps((PR_IPM_SUBTREE_ENTRYID,), 0)
        subtree_eid = data[0][1]
        folder = folder_store.OpenEntry(subtree_eid, None, mapi.MAPI_DEFERRED_ERRORS)
        for name in folder_names:
            folder = self._FindSubfolder(folder_store, folder, name)
            if folder is None:
                raise ValueError("The subfolder '%s' can not be located" % (name,))
        return folder
    def GetAllItems(self, folder, mapi_flags = None):
        mapi_flags = self._GetMAPIFlags(mapi_flags)
        table = folder.GetContentsTable(0)
        table.SetColumns((PR_ENTRYID,PR_STORE_ENTRYID), 0)
        while 1:
            rows = table.QueryRows(70, 0)
            if len(rows) == 0:
                break
            for row in rows:
                (tag, eid), (tag, store_eid) = row
                store = self.session.OpenMsgStore(0, store_eid, None, mapi_flags)
                item = store.OpenEntry(eid, None, mapi_flags)
                yield item
    def GetItemsWithValue(self, folder, prop_tag, prop_val, mapi_flags = None):
        mapi_flags = self._GetMAPIFlags(mapi_flags)
        tab = folder.GetContentsTable(0)
        restriction = (mapi.RES_CONTENT,   # a property restriction
                       (mapi.FL_SUBSTRING | mapi.FL_IGNORECASE | mapi.FL_LOOSE, # fuzz level
                        prop_tag,   # of the given prop
                        (prop_tag, prop_val))) # with given val
        rows = mapi.HrQueryAllRows(tab,
                                   (PR_ENTRYID, PR_STORE_ENTRYID),   # columns to retrieve
                                   restriction,     # only these rows
                                   None,            # any sort order is fine
                                   0)               # any # of results is fine
        for row in rows:
            (tag, eid),(tag, store_eid) = row
            store = self.session.OpenMsgStore(0, store_eid, None, mapi_flags)
            item = store.OpenEntry(eid, None, mapi_flags)
            yield item
    def DumpTopLevelFolders(self):
        print("Top-level folder names are:")
        for store, name, is_default in self.GetMessageStores():
            hr, data = store.GetProps((PR_IPM_SUBTREE_ENTRYID,), 0)
            subtree_eid = data[0][1]
            folder = store.OpenEntry(subtree_eid, None, mapi.MAPI_DEFERRED_ERRORS)
            table = folder.GetHierarchyTable(0)
            rows = mapi.HrQueryAllRows(table, (PR_DISPLAY_NAME_A), None, None, 0)
            for (name_tag, folder_name), in rows:
                print(" \\%s\\%s" % (name, folder_name))
    def GetFolderNameDoc(self):
        def_store_name = "<??unknown??>"
        for store, name, is_def in self.GetMessageStores():
            if is_def:
                def_store_name = name
        return """\
Folder name is a hierarchical 'path' name, using '\\'
as the path separator.  If the folder name begins with a
\\, it must be a fully-qualified name, including the message
store name. For example, as your default store is currently named
'%s', your Inbox can be specified either as:
  -f "Inbox"
or
  -f "\\%s\\Inbox"
""" % (def_store_name, def_store_name)
if __name__=='__main__':
    print("This is a utility script for the other scripts in this directory")
