from __future__ import generators
import sys, os, re
import locale
from time import timezone
import email
from email.MIMEImage import MIMEImage
from email.Message import Message
from email.MIMEMultipart import MIMEMultipart
from email.MIMEText import MIMEText
from email.Parser import HeaderParser
from email.Utils import formatdate
try:
    from cStringIO import StringIO
except ImportError:
    from StringIO import StringIO
from win32com.client import Dispatch, constants
from win32com.mapi import mapi, mapiutil
from win32com.mapi.mapitags import *
import pythoncom
import winerror
MESSAGE_MOVE = 0x1 
MSGFLAG_READ = 0x1 
MSGFLAG_UNSENT = 0x00000008
MYPR_BODY_HTML_A = 0x1013001e 
MYPR_BODY_HTML_W = 0x1013001f 
MYPR_MESSAGE_ID_A = 0x1035001E 
CLEAR_READ_FLAG = 0x00000004
CLEAR_RN_PENDING = 0x00000020
CLEAR_NRN_PENDING = 0x00000040
SUPPRESS_RECEIPT = 0x1
FOLDER_DIALOG = 0x00000002
USE_DEFERRED_ERRORS = mapi.MAPI_DEFERRED_ERRORS 
test_suite_running = False
test_suite_failure_request = None
test_suite_failure = None
test_suite_failure_count = None
def help_test_suite(checkpoint_name):
    global test_suite_failure_request, test_suite_failure_count
    if test_suite_running and \
       test_suite_failure_request == checkpoint_name:
        if test_suite_failure_count:
            test_suite_failure_count -= 1
            if test_suite_failure_count==0:
                test_suite_failure_request = None
        raise test_suite_failure[0], test_suite_failure[1]
class MsgStoreException(Exception):
    def __init__(self, mapi_exception, extra_msg = None):
        self.mapi_exception = mapi_exception
        self.extra_msg = extra_msg
        Exception.__init__(self, mapi_exception, extra_msg)
    def __str__(self):
        try:
            if self.mapi_exception is not None:
                err_str = GetCOMExceptionString(self.mapi_exception)
            else:
                err_str = self.extra_msg or ''
            return "%s: %s" % (self.__class__.__name__, err_str)
        except:
            print "FAILED to str() a MsgStore exception!"
            import traceback
            traceback.print_exc()
class NotFoundException(MsgStoreException):
    pass
class ReadOnlyException(MsgStoreException):
    pass
class ObjectChangedException(MsgStoreException):
    pass
def MsgStoreExceptionFromCOMException(com_exc):
    if IsNotFoundCOMException(com_exc):
        return NotFoundException(com_exc)
    if IsReadOnlyCOMException(com_exc):
        return ReadOnlyException(com_exc)
    scode = NormalizeCOMException(com_exc)[0]
    if scode == mapi.MAPI_E_OBJECT_CHANGED:
        return ObjectChangedException(com_exc)
    return MsgStoreException(com_exc)
def NormalizeCOMException(exc_val):
    hr, msg, exc, arg_err = exc_val
    if hr == winerror.DISP_E_EXCEPTION and exc:
        wcode, source, msg, help1, help2, hr = exc
    return hr, msg, exc, arg_err
def GetCOMExceptionString(exc_val):
    hr, msg, exc, arg_err = NormalizeCOMException(exc_val)
    err_string = mapiutil.GetScodeString(hr)
    return "Exception 0x%x (%s): %s" % (hr, err_string, msg)
def IsNotFoundCOMException(exc_val):
    hr, msg, exc, arg_err = NormalizeCOMException(exc_val)
    return hr in [mapi.MAPI_E_OBJECT_DELETED, mapi.MAPI_E_NOT_FOUND]
def IsNotAvailableCOMException(exc_val):
    hr, msg, exc, arg_err = NormalizeCOMException(exc_val)
    return hr == mapi.MAPI_E_FAILONEPROVIDER
def IsReadOnlyCOMException(exc_val):
    known_failure_codes = -2146644781, -2147164169
    exc_val = NormalizeCOMException(exc_val)
    return exc_val[0] in known_failure_codes
def ReportMAPIError(manager, what, exc_val):
    hr, exc_msg, exc, arg_err = exc_val
    if hr == mapi.MAPI_E_TABLE_TOO_BIG:
        err_msg = what + _(" failed as one of your\r\n" \
                    "Outlook folders is full.  Futher operations are\r\n" \
                    "likely to fail until you clean up this folder.\r\n\r\n" \
                    "This message will not be reported again until SpamBayes\r\n"\
                    "is restarted.")
    else:
        err_msg = what + _(" failed due to an unexpected Outlook error.\r\n") \
                  + GetCOMExceptionString(exc_val) + "\r\n\r\n" + \
                  _("It is recommended you restart Outlook at the earliest opportunity\r\n\r\n" \
                    "This message will not be reported again until SpamBayes\r\n"\
                    "is restarted.")
    manager.ReportErrorOnce(err_msg)
class MAPIMsgStore:
    MsgStoreException = MsgStoreException
    NotFoundException = NotFoundException
    ReadOnlyException = ReadOnlyException
    ObjectChangedException = ObjectChangedException
    def __init__(self, outlook = None):
        self.outlook = outlook
        cwd = os.getcwd() 
        mapi.MAPIInitialize(None)
        logonFlags = (mapi.MAPI_NO_MAIL |
                      mapi.MAPI_EXTENDED |
                      mapi.MAPI_USE_DEFAULT)
        self.session = mapi.MAPILogonEx(0, None, None, logonFlags)
        locale.setlocale(locale.LC_NUMERIC, "C")
        self.mapi_msg_stores = {}
        self.default_store_bin_eid = None
        os.chdir(cwd)
    def Close(self):
        self.mapi_msg_stores = None
        self.session.Logoff(0, 0, 0)
        self.session = None
        mapi.MAPIUninitialize()
    def GetProfileName(self):
        try:
            self.session.GetStatusTable
        except AttributeError:
            return None
        MAPI_SUBSYSTEM = 39
        restriction = mapi.RES_PROPERTY, (mapi.RELOP_EQ, PR_RESOURCE_TYPE,
                                          (PR_RESOURCE_TYPE, MAPI_SUBSYSTEM))
        table = self.session.GetStatusTable(0)
        rows = mapi.HrQueryAllRows(table,
                                    (PR_DISPLAY_NAME_A,),   
                                    restriction,     
                                    None,            
                                    0)               
        assert len(rows)==1, "Should be exactly one row"
        (tag, val), = rows[0]
        return val.decode("mbcs", "ignore")
    def _GetMessageStore(self, store_eid): 
        try:
            return self.mapi_msg_stores[store_eid]
        except KeyError:
            pass
        given_store_eid = store_eid
        if store_eid is None:
            tab = self.session.GetMsgStoresTable(0)
            restriction = (mapi.RES_PROPERTY,   
                           (mapi.RELOP_EQ,      
                            PR_DEFAULT_STORE,   
                            (PR_DEFAULT_STORE, True))) 
            rows = mapi.HrQueryAllRows(tab,
                                       (PR_ENTRYID,),   
                                       restriction,     
                                       None,            
                                       0)               
            row = rows[0]
            eid_tag, store_eid = row[0]
            self.default_store_bin_eid = store_eid
        store = self.session.OpenMsgStore(
                                0,      
                                store_eid,    
                                None,   
                                mapi.MDB_WRITE |
                                    mapi.MDB_NO_MAIL |
                                    USE_DEFERRED_ERRORS)
        self.mapi_msg_stores[store_eid] = store
        if given_store_eid is None: 
            self.mapi_msg_stores[None] = store
        return store
    def GetRootFolder(self, store_id = None):
        store = self._GetMessageStore(store_id)
        hr, data = store.GetProps((PR_ENTRYID, PR_IPM_SUBTREE_ENTRYID), 0)
        store_eid = data[0][1]
        subtree_eid = data[1][1]
        eid = mapi.HexFromBin(store_eid), mapi.HexFromBin(subtree_eid)
        return self.GetFolder(eid)
    def _OpenEntry(self, id, iid = None, flags = None):
        store_id, item_id = id
        store = self._GetMessageStore(store_id)
        if flags is None:
            flags = mapi.MAPI_MODIFY | USE_DEFERRED_ERRORS
        return store.OpenEntry(item_id, iid, flags)
    def NormalizeID(self, item_id):
        assert type(item_id)==type(()), \
               "Item IDs must be a tuple (not a %r)" % item_id
        try:
            store_id, entry_id = item_id
            return mapi.BinFromHex(store_id), mapi.BinFromHex(entry_id)
        except ValueError:
            raise MsgStoreException(None, "The specified ID '%s' is invalid" % (item_id,))
    def _GetSubFolderIter(self, folder):
        table = folder.GetHierarchyTable(0)
        rows = mapi.HrQueryAllRows(table,
                                   (PR_ENTRYID, PR_STORE_ENTRYID, PR_DISPLAY_NAME_A),
                                   None,
                                   None,
                                   0)
        for (eid_tag, eid), (store_eid_tag, store_eid), (name_tag, name) in rows:
            item_id = store_eid, eid
            sub = self._OpenEntry(item_id)
            table = sub.GetContentsTable(0)
            yield MAPIMsgStoreFolder(self, item_id, name, table.GetRowCount(0))
            for store_folder in self._GetSubFolderIter(sub):
                yield store_folder
    def GetFolderGenerator(self, folder_ids, include_sub):
        for folder_id in folder_ids:
            try:
                folder_id = self.NormalizeID(folder_id)
            except MsgStoreException, details:
                print "NOTE: Skipping invalid folder", details
                continue
            try:
                folder = self._OpenEntry(folder_id)
                table = folder.GetContentsTable(0)
            except pythoncom.com_error, details:
                if IsNotAvailableCOMException(details):
                    print "NOTE: Skipping folder for this session - temporarily unavailable"
                elif IsNotFoundCOMException(details):
                    print "NOTE: Skipping deleted folder"
                else:
                    print "WARNING: Unexpected MAPI error opening folder"
                    print GetCOMExceptionString(details)
                continue
            rc, props = folder.GetProps( (PR_DISPLAY_NAME_A,), 0)
            yield MAPIMsgStoreFolder(self, folder_id, props[0][1],
                                     table.GetRowCount(0))
            if include_sub:
                for f in self._GetSubFolderIter(folder):
                    yield f
    def GetFolder(self, folder_id):
        try: 
            try:
                sid = mapi.BinFromHex(folder_id.StoreID)
                eid = mapi.BinFromHex(folder_id.EntryID)
                folder_id = sid, eid
            except AttributeError:
                folder_id = self.NormalizeID(folder_id)
            folder = self._OpenEntry(folder_id)
            table = folder.GetContentsTable(0)
            rc, props = folder.GetProps( (PR_ENTRYID, PR_DISPLAY_NAME_A), 0)
            folder_id = folder_id[0], props[0][1]
            return MAPIMsgStoreFolder(self, folder_id, props[1][1],
                                  table.GetRowCount(0))
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
    def GetMessage(self, message_id):
        try: 
            try:
                eid = mapi.BinFromHex(message_id.EntryID)
                sid = mapi.BinFromHex(message_id.Parent.StoreID)
                message_id = sid, eid
            except AttributeError:
                message_id = self.NormalizeID(message_id)
            mapi_object = self._OpenEntry(message_id)
            hr, data = mapi_object.GetProps(MAPIMsgStoreMsg.message_init_props,0)
            return MAPIMsgStoreMsg(self, data)
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
    def YieldReceiveFolders(self, msg_class = "IPM.Note"):
        tab = self.session.GetMsgStoresTable(0)
        rows = mapi.HrQueryAllRows(tab,
                                    (PR_ENTRYID,),   
                                    None,            
                                    None,            
                                    0)               
        for row in rows:
            eid_tag, store_eid = row[0]
            try:
                store = self._GetMessageStore(store_eid)
                folder_eid, ret_class = store.GetReceiveFolder(msg_class, 0)
                hex_folder_eid = mapi.HexFromBin(folder_eid)
                hex_store_eid = mapi.HexFromBin(store_eid)
            except pythoncom.com_error, details:
                if not IsNotAvailableCOMException(details):
                    print "ERROR enumerating a receive folder -", details
                continue
            try:
                folder = self.GetFolder((hex_store_eid, hex_folder_eid))
                if folder.GetParent() is not None:
                    yield folder
            except MsgStoreException, details:
                print "ERROR opening receive folder -", details
                continue
_MapiTypeMap = {
    type(0.0): PT_DOUBLE,
    type(0): PT_I4,
    type(''): PT_STRING8,
    type(u''): PT_UNICODE,
}
def GetPropFromStream(mapi_object, prop_id):
    try:
        stream = mapi_object.OpenProperty(prop_id,
                                          pythoncom.IID_IStream,
                                          0, 0)
        chunks = []
        while 1:
            chunk = stream.Read(4096)
            if not chunk:
                break
            chunks.append(chunk)
        return "".join(chunks)
    except pythoncom.com_error, d:
        print "Error getting property", mapiutil.GetPropTagName(prop_id), \
              "from stream:", d
        return ""
def GetPotentiallyLargeStringProp(mapi_object, prop_id, row):
    got_tag, got_val = row
    if PROP_TYPE(got_tag) == PT_ERROR:
        ret = ""
        if got_val == mapi.MAPI_E_NOT_FOUND:
            pass 
        elif got_val == mapi.MAPI_E_NOT_ENOUGH_MEMORY:
            ret = GetPropFromStream(mapi_object, prop_id)
        else:
            tag_name = mapiutil.GetPropTagName(prop_id)
            err_string = mapiutil.GetScodeString(got_val)
            print "Warning - failed to get property %s: %s" % (tag_name,
                                                                err_string)
    else:
        ret = got_val
    return ret
def GetHTMLFromRTFProperty(mapi_object, prop_tag = PR_RTF_COMPRESSED):
    try:
        rtf_stream = mapi_object.OpenProperty(prop_tag, pythoncom.IID_IStream,
                                              0, 0)
        html_stream = mapi.WrapCompressedRTFStream(rtf_stream, 0)
        html = mapi.RTFStreamToHTML(html_stream)
    except pythoncom.com_error, details:
        if not IsNotFoundCOMException(details):
            print "ERROR getting RTF body", details
        return ""
    return html or ''
class MAPIMsgStoreFolder:
    def __init__(self, msgstore, id, name, count):
        self.msgstore = msgstore
        self.id = id
        self.name = name
        self.count = count
    def __repr__(self):
        return "<%s '%s' (%d items), id=%s/%s>" % (self.__class__.__name__,
                                                self.name,
                                                self.count,
                                                mapi.HexFromBin(self.id[0]),
                                                mapi.HexFromBin(self.id[1]))
    def __eq__(self, other):
        if other is None: return False
        ceid = self.msgstore.session.CompareEntryIDs
        return ceid(self.id[0], other.id[0]) and \
               ceid(self.id[1], other.id[1])
    def __ne__(self, other):
        return not self.__eq__(other)
    def GetID(self):
        return mapi.HexFromBin(self.id[0]), mapi.HexFromBin(self.id[1])
    def GetFQName(self):
        parts = []
        parent = self
        while parent is not None:
            parts.insert(0, parent.name)
            try:
                parent = parent.GetParent()
            except MsgStoreException:
                break
        if parts and not parts[0]:
            del parts[0]
        mapi_store = self.msgstore._GetMessageStore(self.id[0])
        hr, data = mapi_store.GetProps((PR_DISPLAY_NAME_A,), 0)
        name = data[0][1]
        if parts:
            parts[0] = name
        else:
            parts = [name]
            print "WARNING: It appears you are using the top-level root of " \
                  "the information store as a folder.  You probably don't "\
                  "want to do that"
        return "/".join(parts)
    def _FolderFromMAPIFolder(self, mapifolder):
        hr, data = mapifolder.GetProps((PR_ENTRYID, PR_DISPLAY_NAME_A,), 0)
        eid = self.id[0], data[0][1]
        name = data[1][1]
        count = mapifolder.GetContentsTable(0).GetRowCount(0)
        return MAPIMsgStoreFolder(self.msgstore, eid, name, count)
    def GetParent(self):
        try:
            folder = self.msgstore._OpenEntry(self.id)
            prop_ids = PR_PARENT_ENTRYID,
            hr, data = folder.GetProps(prop_ids,0)
            parent_eid = data[0][1]
            parent_id = self.id[0], parent_eid
            if hr != 0 or \
               self.msgstore.session.CompareEntryIDs(parent_eid, self.id[1]):
                return None
            parent = self.msgstore._OpenEntry(parent_id)
            return self._FolderFromMAPIFolder(parent)
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
    def OpenEntry(self, iid = None, flags = None):
        return self.msgstore._OpenEntry(self.id, iid, flags)
    def GetOutlookItem(self):
        try:
            hex_item_id = mapi.HexFromBin(self.id[1])
            hex_store_id = mapi.HexFromBin(self.id[0])
            return self.msgstore.outlook.Session.GetFolderFromID(hex_item_id, hex_store_id)
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
    def GetMessageGenerator(self, only_filter_candidates = True):
        folder = self.OpenEntry()
        table = folder.GetContentsTable(0)
        table.SetColumns(MAPIMsgStoreMsg.message_init_props, 0)
        if only_filter_candidates:
            restriction = (mapi.RES_PROPERTY,   
                           (mapi.RELOP_GE,      
                            PR_MESSAGE_CLASS_A,   
                            (PR_MESSAGE_CLASS_A, "IPM."))) 
            table.Restrict(restriction, 0)
        while 1:
            rows = table.QueryRows(70, 0)
            if len(rows) == 0:
                break
            for row in rows:
                msg = MAPIMsgStoreMsg(self.msgstore, row)
                if not only_filter_candidates or msg.IsFilterCandidate():
                    yield msg
    def GetNewUnscoredMessageGenerator(self, scoreFieldName):
        folder = self.msgstore._OpenEntry(self.id)
        table = folder.GetContentsTable(0)
        resolve_props = ( (mapi.PS_PUBLIC_STRINGS, scoreFieldName), )
        resolve_ids = folder.GetIDsFromNames(resolve_props, 0)
        field_id = PROP_TAG( PT_DOUBLE, PROP_ID(resolve_ids[0]))
        table.SetColumns(MAPIMsgStoreMsg.message_init_props, 0)
        prop_restriction = (mapi.RES_BITMASK,   
                               (mapi.BMR_EQZ,      
                                PR_MESSAGE_FLAGS,
                                MSGFLAG_READ))
        exist_restriction = mapi.RES_EXIST, (field_id,)
        not_exist_restriction = mapi.RES_NOT, (exist_restriction,)
        class_restriction = (mapi.RES_PROPERTY,   
                             (mapi.RELOP_GE,      
                              PR_MESSAGE_CLASS_A,   
                              (PR_MESSAGE_CLASS_A, "IPM."))) 
        restriction = (mapi.RES_AND, (prop_restriction,
                                      not_exist_restriction,
                                      class_restriction))
        table.Restrict(restriction, 0)
        while 1:
            rows = table.QueryRows(70, 0)
            if len(rows) == 0:
                break
            for row in rows:
                msg = MAPIMsgStoreMsg(self.msgstore, row)
                if msg.IsFilterCandidate():
                    yield msg
    def IsReceiveFolder(self, msg_class = "IPM.Note"):
        try:
            mapi_store = self.msgstore._GetMessageStore(self.id[0])
            eid, ret_class = mapi_store.GetReceiveFolder(msg_class, 0)
            return mapi_store.CompareEntryIDs(eid, self.id[1])
        except pythoncom.com_error:
            return False
    def CreateFolder(self, name, comments = None, type = None,
                     open_if_exists = False, flags = None):
        if type is None: type = mapi.FOLDER_GENERIC
        if flags is None: flags = 0
        if open_if_exists: flags |= mapi.OPEN_IF_EXISTS
        folder = self.OpenEntry()
        ret = folder.CreateFolder(type, name, comments, None, flags)
        return self._FolderFromMAPIFolder(ret)
    def GetItemCount(self):
        try:
            folder = self.OpenEntry()
            return folder.GetContentsTable(0).GetRowCount(0)
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
    def EmptyFolder(self, parentWindow):
        try:
            folder = self.OpenEntry()
            folder.EmptyFolder(parentWindow, None, FOLDER_DIALOG)
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
    def DoesFolderHaveOutlookField(self, field_name):
        try:
            folder = self.msgstore._OpenEntry(self.id)
            table = folder.GetContentsTable(mapi.MAPI_ASSOCIATED)
            restriction = (mapi.RES_PROPERTY,
                          (mapi.RELOP_EQ,
                           PR_MESSAGE_CLASS_A,
                           (PR_MESSAGE_CLASS_A, 'IPC.MS.REN.USERFIELDS')))
            cols = (PR_USERFIELDS,)
            table.SetColumns(cols, 0)
            rows = mapi.HrQueryAllRows(table, cols, restriction, None, 0)
            if len(rows)>1:
                print "Eeek - only expecting one row from IPC.MS.REN.USERFIELDS"
                print "got", repr(rows)
                return None
            if len(rows)==0:
                return False
            row = rows[0]
            val = GetPotentiallyLargeStringProp(folder, cols[0], row[0])
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
        if type(val) != type(''):
            print "Value type incorrect - expected string, got", repr(val)
            return None
        return val.find("\0" + field_name) >= 0
    def DeleteMessages(self, message_things):
        real_ids = []
        for thing in message_things:
            if isinstance(thing, MAPIMsgStoreMsg):
                real_ids.append( thing.id[1] )
                thing.mapi_object = thing.id = thing.folder_id = None
            else:
                real_ids.append(self.msgstore.NormalizeID(thing)[1])
        try:
            folder = self.msgstore._OpenEntry(self.id)
            folder.DeleteMessages(real_ids, 0, None, 0)
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
    def CreateTemporaryMessage(self, msg_flags = None):
        try:
            folder = self.msgstore._OpenEntry(self.id)
            imsg = folder.CreateMessage(None, 0)
            if msg_flags is not None:
                props = (PR_MESSAGE_FLAGS,msg_flags),
                imsg.SetProps(props)
            imsg.SaveChanges(0)
            hr, data = imsg.GetProps((PR_ENTRYID, PR_STORE_ENTRYID), 0)
            eid = data[0][1]
            storeid = data[1][1]
            msg_id = mapi.HexFromBin(storeid), mapi.HexFromBin(eid)
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
        return self.msgstore.GetMessage(msg_id)
class MAPIMsgStoreMsg:
    message_init_props = (PR_ENTRYID, PR_STORE_ENTRYID, PR_SEARCH_KEY,
                          PR_PARENT_ENTRYID, 
                          PR_MESSAGE_CLASS_A, 
                          PR_RECEIVED_BY_ENTRYID, 
                          PR_SUBJECT_A,
                          PR_TRANSPORT_MESSAGE_HEADERS_A,
                          )
    def __init__(self, msgstore, prop_row):
        self.msgstore = msgstore
        self.mapi_object = None
        tag, eid = prop_row[0] 
        tag, store_eid = prop_row[1]
        tag, searchkey = prop_row[2]
        tag, parent_eid = prop_row[3]
        tag, msgclass = prop_row[4]
        recby_tag, recby = prop_row[5]
        tag, subject = prop_row[6]
        headers_tag, headers = prop_row[7]
        self.id = store_eid, eid
        self.folder_id = store_eid, parent_eid
        self.msgclass = msgclass
        self.subject = subject
        has_headers = PROP_TYPE(headers_tag)==PT_STRING8
        self.searchkey = searchkey
        self.was_received = PROP_TYPE(recby_tag) == PT_BINARY or has_headers
        self.dirty = False
        self.stored_attributes = ['c', 't', 'original_folder',
                                  'date_modified']
        self.t = None
        self.c = None
        self.date_modified = None
        self.original_folder = None
    def getDBKey(self):
        return self.searchkey
    def __repr__(self):
        if self.id is None:
            id_str = "(deleted/moved)"
        else:
            id_str = mapi.HexFromBin(self.id[0]), mapi.HexFromBin(self.id[1])
        return "<%s, '%s' id=%s>" % (self.__class__.__name__,
                                     self.GetSubject(),
                                     id_str)
    def __hash__(self):
        return hash(self.searchkey)
    def __eq__(self, other):
        ceid = self.msgstore.session.CompareEntryIDs
        return ceid(self.searchkey, other.searchkey)
    def __ne__(self, other):
        return not self.__eq__(other)
    def GetID(self):
        return mapi.HexFromBin(self.id[0]), mapi.HexFromBin(self.id[1])
    def GetSubject(self):
        return self.subject
    def GetOutlookItem(self):
        hex_item_id = mapi.HexFromBin(self.id[1])
        hex_store_id = mapi.HexFromBin(self.id[0])
        return self.msgstore.outlook.Session.GetItemFromID(hex_item_id, hex_store_id)
    def IsFilterCandidate(self):
        if test_suite_running:
            return self.subject == "SpamBayes addin auto-generated test message"
        class_check = self.msgclass.lower()
        for check in "ipm.note", "ipm.anti-virus":
            if class_check.startswith(check):
                break
        else:
            return False
        return self.was_received
    def _GetPotentiallyLargeStringProp(self, prop_id, row):
        return GetPotentiallyLargeStringProp(self.mapi_object, prop_id, row)
    def _GetMessageText(self):
        parts = self._GetMessageTextParts()
        return "\n".join(parts)
    def _GetMessageTextParts(self):
        from spambayes import mboxutils
        self._EnsureObject()
        prop_ids = (PR_BODY_A,
                    MYPR_BODY_HTML_A,
                    PR_TRANSPORT_MESSAGE_HEADERS_A)
        hr, data = self.mapi_object.GetProps(prop_ids,0)
        body = self._GetPotentiallyLargeStringProp(prop_ids[0], data[0])
        html = self._GetPotentiallyLargeStringProp(prop_ids[1], data[1])
        headers = self._GetPotentiallyLargeStringProp(prop_ids[2], data[2])
        if not html:
            html = GetHTMLFromRTFProperty(self.mapi_object)
        headers = mboxutils.extract_headers(headers)
        if not headers:
            headers = self._GetFakeHeaders()
        elif headers.startswith("Microsoft Mail"):
            headers = "X-MS-Mail-Gibberish: " + headers
            if headers.find("Received:") == -1:
                prop_ids = PR_MESSAGE_DELIVERY_TIME
                hr, data = self.mapi_object.GetProps(prop_ids, 0)
                value = self._format_received(data[0][1])
                headers = "Received: %s\n%s" % (value, headers)
        if not html and not body:
            table = self.mapi_object.GetAttachmentTable(0)
            restriction = (mapi.RES_PROPERTY,   
                           (mapi.RELOP_EQ,      
                            PR_ATTACH_MIME_TAG_A,   
                            (PR_ATTACH_MIME_TAG_A, "multipart/signed")))
            try:
                rows = mapi.HrQueryAllRows(table,
                                           (PR_ATTACH_NUM,), 
                                           restriction,    
                                           None,    
                                           0)       
            except pythoncom.com_error:
                rows = []
            if len(rows) == 0:
                pass 
            else:
                if len(rows) > 1:
                    print "WARNING: Found %d rows with multipart/signed" \
                          "- using first only" % len(rows)
                row = rows[0]
                (attach_num_tag, attach_num), = row
                assert attach_num_tag != PT_ERROR, \
                       "Error fetching attach_num prop"
                attach = self.mapi_object.OpenAttach(attach_num,
                                                   None,
                                                   mapi.MAPI_DEFERRED_ERRORS)
                prop_ids = (PR_ATTACH_DATA_BIN,)
                hr, data = attach.GetProps(prop_ids, 0)
                attach_body = GetPotentiallyLargeStringProp(attach, prop_ids[0], data[0])
                msg = email.message_from_string(attach_body)
                assert msg.is_multipart(), "Should be multi-part: %r" % attach_body
                def collect_text_parts(msg):
                    collected = ''
                    if msg.is_multipart():
                        for sub in msg.get_payload():
                            collected += collect_text_parts(sub)
                    else:
                        if msg.get_content_maintype()=='text':
                            collected += msg.get_payload()
                        else:
                            pass
                    return collected
                body = collect_text_parts(msg)
        return headers, body, html
    def _GetFakeHeaders(self):
        prop_ids = PR_SUBJECT_A, PR_SENDER_NAME_A, PR_DISPLAY_TO_A, \
                   PR_DISPLAY_CC_A, PR_MESSAGE_DELIVERY_TIME, \
                   MYPR_MESSAGE_ID_A, PR_IMPORTANCE, PR_CLIENT_SUBMIT_TIME,
        hr, data = self.mapi_object.GetProps(prop_ids, 0)
        headers = ["X-Exchange-Message: true"]
        for header, index, potentially_large, format_func in (\
            ("Subject", 0, True, None),
            ("From", 1, True, self._format_address),
            ("To", 2, True, self._format_address),
            ("CC", 3, True, self._format_address),
            ("Received", 4, False, self._format_received),
            ("Message-ID", 5, True, None),
            ("Importance", 6, False, self._format_importance),
            ("Date", 7, False, self._format_time),
            ("X-Mailer", 7, False, self._format_version),
            ):
            if potentially_large:
                value = self._GetPotentiallyLargeStringProp(prop_ids[index],
                                                            data[index])
            else:
                value = data[index][1]
            if value:
                if format_func:
                    value = format_func(value)
                headers.append("%s: %s" % (header, value))
        return "\n".join(headers) + "\n"
    def _format_received(self, raw):
        return "(via local Exchange server); %s" % (self._format_time(raw),)
    def _format_time(self, raw):
        return formatdate(int(raw)-timezone, True)
    def _format_importance(self, raw):
        return {0 : "low", 1 : "normal", 2 : "high"}[raw]
    def _format_version(self, unused):
        return "Microsoft Exchange Client"
    _address_re = re.compile(r"[()<>,:@!/=; ]")
    def _format_address(self, raw):
        addresses = raw.split(";")
        formattedAddresses = []
        for address in addresses:
            address = address.strip()
            if address.find("@") >= 0:
                formattedAddress = address
            else:
                formattedAddress = "\"%s\" <%s>" % \
                        (address, self._address_re.sub('.', address))
            formattedAddresses.append(formattedAddress)
        return "; ".join(formattedAddresses)
    def _EnsureObject(self):
        if self.mapi_object is None:
            try:
                help_test_suite("MAPIMsgStoreMsg._EnsureObject")
                self.mapi_object = self.msgstore._OpenEntry(self.id)
            except pythoncom.com_error, details:
                raise MsgStoreExceptionFromCOMException(details)
    def _GetAttachmentsToInclude(self):
        from spambayes.Options import options
        from spambayes.ImageStripper import image_large_size_attribute
        if not options['Tokenizer', 'crack_images'] and \
           not options['Tokenizer', 'image_size']:
            return []
        try:
            table = self.mapi_object.GetAttachmentTable(0)
            tags = PR_ATTACH_NUM,PR_ATTACH_MIME_TAG_A,PR_ATTACH_SIZE,PR_ATTACH_DATA_BIN
            attach_rows = mapi.HrQueryAllRows(table, tags, None, None, 0)
        except pythoncom.com_error, why:
            attach_rows = []
        attachments = []
        for row in attach_rows:
            attach_num = row[0][1]
            mime_tag = None
            if PROP_TYPE(row[1][0]) != PT_ERROR:
                mime_tag = row[1][1]
            if mime_tag:
                typ, subtyp = mime_tag.split('/', 1)
                if typ == 'image':
                    size = row[2][1]
                    if size > options["Tokenizer", "max_image_size"]:
                        sub = MIMEImage(None, subtyp)
                        setattr(sub, image_large_size_attribute, size)
                    else:
                        attach = self.mapi_object.OpenAttach(attach_num,
                                        None, mapi.MAPI_DEFERRED_ERRORS)
                        data = GetPotentiallyLargeStringProp(attach,
                                    PR_ATTACH_DATA_BIN, row[3])
                        sub = MIMEImage(data, subtyp)
                    attachments.append(sub)
        return attachments
    def GetEmailPackageObject(self, strip_mime_headers=True):
        header_text, body, html = self._GetMessageTextParts()
        try: 
            attachments = self._GetAttachmentsToInclude()
            new_content_type = None
            if attachments:
                _class = MIMEMultipart
                payload = []
                if body:
                    payload.append(MIMEText(body))
                if html:
                    payload.append(MIMEText(html, 'html'))
                payload += attachments
                new_content_type = "multipart/mixed"
            else:
                _class = Message
                payload = body + '\n' + html
            try:
                root_msg = HeaderParser(_class=_class).parsestr(header_text)
            except email.Errors.HeaderParseError:
                raise 
            if strip_mime_headers:
                for h, new_val in (('content-type', new_content_type),
                                   ('content-transfer-encoding', None)):
                    try:
                        root_msg['X-SpamBayes-Original-' + h] = root_msg[h]
                        del root_msg[h]
                    except KeyError:
                        pass
                    if new_val is not None:
                        root_msg[h] = new_val
            root_msg.set_payload(payload)
        except:
            text = '\r\n'.join([header_text, body, html])
            print "FAILED to create email.message from: ", `text`
            raise
        return root_msg
    def OldGetEmailPackageObject(self, strip_mime_headers=True):
        import email
        text = self._GetMessageText()
        try:
            try:
                msg = email.message_from_string(text)
            except email.Errors.BoundaryError:
                try:
                    msg = email.message_from_string(text + "\n\n")
                except email.Errors.BoundaryError:
                    msg = None
            except email.Errors.HeaderParseError:
                msg = None
            if msg is None:
                butcher_pos = text.lower().find("\ncontent-type: ")
                if butcher_pos < 0:
                    raise RuntimeError(
                        "email package croaked with a MIME related error, but "
                        "there appears to be no 'Content-Type' header")
                butchered = text[:butcher_pos] + "\nSpamBayes-" + \
                            text[butcher_pos+1:] + "\n\n"
                msg = email.message_from_string(butchered)
        except:
            print "FAILED to create email.message from: ", `text`
            raise
        if strip_mime_headers:
            if msg.has_key('content-type'):
                del msg['content-type']
            if msg.has_key('content-transfer-encoding'):
                del msg['content-transfer-encoding']
        return msg
    def SetField(self, prop, val):
        self._EnsureObject()
        try:
            if type(prop) != type(0):
                props = ( (mapi.PS_PUBLIC_STRINGS, prop), )
                propIds = self.mapi_object.GetIDsFromNames(props, mapi.MAPI_CREATE)
                type_tag = _MapiTypeMap.get(type(val))
                if type_tag is None:
                    raise ValueError, "Don't know what to do with '%r' ('%s')" % (
                                         val, type(val))
                prop = PROP_TAG(type_tag, PROP_ID(propIds[0]))
            help_test_suite("MAPIMsgStoreMsg.SetField")
            if val is None:
                self.mapi_object.DeleteProps((prop,))
            else:
                self.mapi_object.SetProps(((prop,val),))
            self.dirty = True
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
    def GetField(self, prop):
        self._EnsureObject()
        if type(prop) != type(0):
            props = ( (mapi.PS_PUBLIC_STRINGS, prop), )
            prop = self.mapi_object.GetIDsFromNames(props, 0)[0]
            if PROP_TYPE(prop) == PT_ERROR: 
                return None
            prop = PROP_TAG( PT_UNSPECIFIED, PROP_ID(prop))
        try:
            hr, props = self.mapi_object.GetProps((prop,), 0)
            ((tag, val), ) = props
            if PROP_TYPE(tag) == PT_ERROR:
                if val == mapi.MAPI_E_NOT_ENOUGH_MEMORY:
                    return GetPropFromStream(self.mapi_object, prop)
                return None
            return val
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
    def GetReadState(self):
        val = self.GetField(PR_MESSAGE_FLAGS)
        return (val&MSGFLAG_READ) != 0
    def SetReadState(self, is_read):
        try:
            self._EnsureObject()
            help_test_suite("MAPIMsgStoreMsg.SetReadState")
            if is_read:
                self.mapi_object.SetReadFlag(USE_DEFERRED_ERRORS|SUPPRESS_RECEIPT)
            else:
                self.mapi_object.SetReadFlag(USE_DEFERRED_ERRORS|CLEAR_READ_FLAG)
            if __debug__:
                if self.GetReadState() != is_read:
                    print "MAPI SetReadState appears to have failed to change the message state"
                    print "Requested set to %s but the MAPI field after was %r" % \
                          (is_read, self.GetField(PR_MESSAGE_FLAGS))
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
    def Save(self):
        assert self.dirty, "asking me to save a clean message!"
        try:
            help_test_suite("MAPIMsgStoreMsg.Save")
            self.mapi_object.SaveChanges(mapi.KEEP_OPEN_READWRITE)
            self.dirty = False
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
    def _DoCopyMove(self, folder, isMove):
        assert not self.dirty, \
               "asking me to move a dirty message - later saves will fail!"
        try:
            dest_folder = self.msgstore._OpenEntry(folder.id)
            source_folder = self.msgstore._OpenEntry(self.folder_id)
            flags = 0
            if isMove: flags |= MESSAGE_MOVE
            eid = self.id[1]
            help_test_suite("MAPIMsgStoreMsg._DoCopyMove")
            source_folder.CopyMessages((eid,),
                                        None,
                                        dest_folder,
                                        0,
                                        None,
                                        flags)
            self.id = None
            self.folder_id = None
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
    def MoveTo(self, folder):
        self._DoCopyMove(folder, True)
    def CopyTo(self, folder):
        self._DoCopyMove(folder, False)
    def MoveToReportingError(self, manager, folder):
        try:
            self.MoveTo(folder)
        except MsgStoreException, details:
            ReportMAPIError(manager, _("Moving a message"),
                            details.mapi_exception)
    def CopyToReportingError(self, manager, folder):
        try:
            self.MoveTo(folder)
        except MsgStoreException, details:
            ReportMAPIError(manager, _("Copying a message"),
                            details.mapi_exception)
    def GetFolder(self):
        folder_id = (mapi.HexFromBin(self.folder_id[0]),
                     mapi.HexFromBin(self.folder_id[1]))
        return self.msgstore.GetFolder(folder_id)
    def RememberMessageCurrentFolder(self):
        self._EnsureObject()
        try:
            folder = self.GetFolder()
            self.original_folder = folder.id[0], folder.id[1]
            props = ( (mapi.PS_PUBLIC_STRINGS, "SpamBayesOriginalFolderStoreID"),
                      (mapi.PS_PUBLIC_STRINGS, "SpamBayesOriginalFolderID")
                      )
            resolve_ids = self.mapi_object.GetIDsFromNames(props, mapi.MAPI_CREATE)
            prop_ids = PROP_TAG( PT_BINARY, PROP_ID(resolve_ids[0])), \
                       PROP_TAG( PT_BINARY, PROP_ID(resolve_ids[1]))
            prop_tuples = (prop_ids[0],folder.id[0]), (prop_ids[1],folder.id[1])
            self.mapi_object.SetProps(prop_tuples)
            self.dirty = True
        except pythoncom.com_error, details:
            raise MsgStoreExceptionFromCOMException(details)
    def GetRememberedFolder(self):
        props = ( (mapi.PS_PUBLIC_STRINGS, "SpamBayesOriginalFolderStoreID"),
                  (mapi.PS_PUBLIC_STRINGS, "SpamBayesOriginalFolderID")
                  )
        try:
            self._EnsureObject()
            resolve_ids = self.mapi_object.GetIDsFromNames(props, mapi.MAPI_CREATE)
            prop_ids = PROP_TAG( PT_BINARY, PROP_ID(resolve_ids[0])), \
                       PROP_TAG( PT_BINARY, PROP_ID(resolve_ids[1]))
            hr, data = self.mapi_object.GetProps(prop_ids,0)
            if hr != 0:
                return None
            (store_tag, store_id), (eid_tag, eid) = data
            folder_id = mapi.HexFromBin(store_id), mapi.HexFromBin(eid)
            help_test_suite("MAPIMsgStoreMsg.GetRememberedFolder")
            return self.msgstore.GetFolder(folder_id)
        except:
            if self.original_folder:
                return self.msgstore.GetFolder(self.original_folder)
            print "Error locating origin of message", self
            return None
def test():
    outlook = Dispatch("Outlook.Application")
    inbox = outlook.Session.GetDefaultFolder(constants.olFolderInbox)
    folder_id = inbox.Parent.StoreID, inbox.EntryID
    store = MAPIMsgStore()
    for folder in store.GetFolderGenerator([folder_id,], True):
        print folder
        for msg in folder.GetMessageGenerator():
            print msg
    store.Close()
if __name__=='__main__':
    test()
