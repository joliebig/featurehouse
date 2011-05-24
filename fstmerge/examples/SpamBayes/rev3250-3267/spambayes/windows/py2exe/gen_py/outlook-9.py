"""Microsoft Outlook 9.0 Object Library"""

makepy_version = '0.4.95'

python_version = 0x20404f0

import win32com.client.CLSIDToClass, pythoncom

import win32com.client.util

from pywintypes import IID

from win32com.client import Dispatch

defaultNamedOptArg=pythoncom.Empty

defaultNamedNotOptArg=pythoncom.Empty

defaultUnnamedArg=pythoncom.Empty

CLSID = IID('{00062FFF-0000-0000-C000-000000000046}')

MajorVersion = 9

MinorVersion = 0

LibraryFlags = 8

LCID = 0x0

class  constants :
	olForward                     =0x2
		olReply                       =0x0
		olReplyAll                    =0x1
		olReplyFolder                 =0x3
		olRespond                     =0x4
		olEmbedOriginalItem           =0x1
		olIncludeOriginalText         =0x2
		olIndentOriginalText          =0x3
		olLinkOriginalItem            =0x4
		olOmitOriginalText            =0x0
		olReplyTickOriginalText       =0x3e8
		olUserPreference              =0x5
		olOpen                        =0x0
		olPrompt                      =0x2
		olSend                        =0x1
		olDontShow                    =0x0
		olMenu                        =0x1
		olMenuAndToolbar              =0x2
		olByReference                 =0x4
		olByValue                     =0x1
		olEmbeddeditem                =0x5
		olOLE                         =0x6
		olBusy                        =0x2
		olFree                        =0x0
		olOutOfOffice                 =0x3
		olTentative                   =0x1
		olFriday                      =0x20
		olMonday                      =0x2
		olSaturday                    =0x40
		olSunday                      =0x1
		olThursday                    =0x10
		olTuesday                     =0x4
		olWednesday                   =0x8
		olFolderCalendar              =0x9
		olFolderContacts              =0xa
		olFolderDeletedItems          =0x3
		olFolderDrafts                =0x10
		olFolderInbox                 =0x6
		olFolderJournal               =0xb
		olFolderNotes                 =0xc
		olFolderOutbox                =0x4
		olFolderSentMail              =0x5
		olFolderTasks                 =0xd
		olAgent                       =0x3
		olDistList                    =0x1
		olForum                       =0x2
		olOrganization                =0x4
		olPrivateDistList             =0x5
		olRemoteUser                  =0x6
		olUser                        =0x0
		olEditorHTML                  =0x2
		olEditorRTF                   =0x3
		olEditorText                  =0x1
		olEditorWord                  =0x4
		olFlagComplete                =0x1
		olFlagMarked                  =0x2
		olNoFlag                      =0x0
		olFolderDisplayFolderOnly     =0x1
		olFolderDisplayNoNavigation   =0x2
		olFolderDisplayNormal         =0x0
		olDefaultRegistry             =0x0
		olFolderRegistry              =0x3
		olOrganizationRegistry        =0x4
		olPersonalRegistry            =0x2
		olFemale                      =0x1
		olMale                        =0x2
		olUnspecified                 =0x0
		olImportanceHigh              =0x2
		olImportanceLow               =0x0
		olImportanceNormal            =0x1
		olDiscard                     =0x1
		olPromptForSave               =0x2
		olSave                        =0x0
		olAppointmentItem             =0x1
		olContactItem                 =0x2
		olDistributionListItem        =0x7
		olJournalItem                 =0x4
		olMailItem                    =0x0
		olNoteItem                    =0x5
		olPostItem                    =0x6
		olTaskItem                    =0x3
		olAssociatedContact           =0x1
		olBCC                         =0x3
		olCC                          =0x2
		olOriginator                  =0x0
		olTo                          =0x1
		olBusiness                    =0x2
		olHome                        =0x1
		olNone                        =0x0
		olOther                       =0x3
		olOptional                    =0x2
		olOrganizer                   =0x0
		olRequired                    =0x1
		olResource                    =0x3
		olMeetingAccepted             =0x3
		olMeetingDeclined             =0x4
		olMeetingTentative            =0x2
		olMeeting                     =0x1
		olMeetingCanceled             =0x5
		olMeetingReceived             =0x3
		olNonMeeting                  =0x0
		olChat                        =0x2
		olNetMeeting                  =0x0
		olNetShow                     =0x1
		olBlue                        =0x0
		olGreen                       =0x1
		olPink                        =0x2
		olWhite                       =0x4
		olYellow                      =0x3
		olAction                      =0x20
		olActions                     =0x21
		olAddressEntries              =0x15
		olAddressEntry                =0x8
		olAddressList                 =0x7
		olAddressLists                =0x14
		olApplication                 =0x0
		olAppointment                 =0x1a
		olAttachment                  =0x5
		olAttachments                 =0x12
		olContact                     =0x28
		olDistributionList            =0x45
		olDocument                    =0x29
		olException                   =0x1e
		olExceptions                  =0x1d
		olExplorer                    =0x22
		olExplorers                   =0x3c
		olFolder                      =0x2
		olFolders                     =0xf
		olFormDescription             =0x25
		olInspector                   =0x23
		olInspectors                  =0x3d
		olItems                       =0x10
		olJournal                     =0x2a
		olLink                        =0x4b
		olLinks                       =0x4c
		olMail                        =0x2b
		olMeetingCancellation         =0x36
		olMeetingRequest              =0x35
		olMeetingResponseNegative     =0x37
		olMeetingResponsePositive     =0x38
		olMeetingResponseTentative    =0x39
		olNamespace                   =0x1
		olNote                        =0x2c
		olOutlookBarGroup             =0x42
		olOutlookBarGroups            =0x41
		olOutlookBarPane              =0x3f
		olOutlookBarShortcut          =0x44
		olOutlookBarShortcuts         =0x43
		olOutlookBarStorage           =0x40
		olPages                       =0x24
		olPanes                       =0x3e
		olPost                        =0x2d
		olPropertyPageSite            =0x46
		olPropertyPages               =0x47
		olRecipient                   =0x4
		olRecipients                  =0x11
		olRecurrencePattern           =0x1c
		olRemote                      =0x2f
		olReport                      =0x2e
		olSelection                   =0x4a
		olSyncObject                  =0x48
		olSyncObjects                 =0x49
		olTask                        =0x30
		olTaskRequest                 =0x31
		olTaskRequestAccept           =0x33
		olTaskRequestDecline          =0x34
		olTaskRequestUpdate           =0x32
		olUserProperties              =0x26
		olUserProperty                =0x27
		olLargeIcon                   =0x0
		olSmallIcon                   =0x1
		olFolderList                  =0x2
		olOutlookBar                  =0x1
		olPreview                     =0x3
		olApptException               =0x3
		olApptMaster                  =0x1
		olApptNotRecurring            =0x0
		olApptOccurrence              =0x2
		olRecursDaily                 =0x0
		olRecursMonthNth              =0x3
		olRecursMonthly               =0x2
		olRecursWeekly                =0x1
		olRecursYearNth               =0x6
		olRecursYearly                =0x5
		olMarkedForCopy               =0x3
		olMarkedForDelete             =0x4
		olMarkedForDownload           =0x2
		olRemoteStatusNone            =0x0
		olUnMarked                    =0x1
		olResponseAccepted            =0x3
		olResponseDeclined            =0x4
		olResponseNone                =0x0
		olResponseNotResponded        =0x5
		olResponseOrganized           =0x1
		olResponseTentative           =0x2
		olDoc                         =0x4
		olHTML                        =0x5
		olMSG                         =0x3
		olRTF                         =0x1
		olTXT                         =0x0
		olTemplate                    =0x2
		olVCal                        =0x7
		olVCard                       =0x6
		olConfidential                =0x3
		olNormal                      =0x0
		olPersonal                    =0x1
		olPrivate                     =0x2
		olAscending                   =0x1
		olDescending                  =0x2
		olSortNone                    =0x0
		olSyncStarted                 =0x1
		olSyncStopped                 =0x0
		olTaskDelegationAccepted      =0x2
		olTaskDelegationDeclined      =0x3
		olTaskDelegationUnknown       =0x1
		olTaskNotDelegated            =0x0
		olDelegatedTask               =0x1
		olNewTask                     =0x0
		olOwnTask                     =0x2
		olFinalStatus                 =0x3
		olUpdate                      =0x2
		olTaskAccept                  =0x2
		olTaskAssign                  =0x1
		olTaskDecline                 =0x3
		olTaskSimple                  =0x0
		olTaskComplete                =0x2
		olTaskDeferred                =0x4
		olTaskInProgress              =0x1
		olTaskNotStarted              =0x0
		olTaskWaiting                 =0x3
		olTrackingDelivered           =0x1
		olTrackingNone                =0x0
		olTrackingNotDelivered        =0x2
		olTrackingNotRead             =0x3
		olTrackingRead                =0x6
		olTrackingRecallFailure       =0x4
		olTrackingRecallSuccess       =0x5
		olTrackingReplied             =0x7
		olCombination                 =0x13
		olCurrency                    =0xe
		olDateTime                    =0x5
		olDuration                    =0x7
		olFormula                     =0x12
		olKeywords                    =0xb
		olNumber                      =0x3
		olPercent                     =0xc
		olText                        =0x1
		olYesNo                       =0x6
		olMaximized                   =0x0
		olMinimized                   =0x1
		olNormalWindow                =0x2
from win32com.client import DispatchBaseClass class  Action (DispatchBaseClass) :
	CLSID = IID('{00063043-0000-0000-C000-000000000046}')
		coclass_clsid = None
		def Delete(self):

		return self._oleobj_.InvokeTypes(108, LCID, 1, (24, 0), (),)
 def Execute(self):

		ret = self._oleobj_.InvokeTypes(106, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Execute', None)

		return ret

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"CopyLike": (100, 2, (3, 0), (), "CopyLike", None),
		"Enabled": (103, 2, (11, 0), (), "Enabled", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Name": (12289, 2, (8, 0), (), "Name", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"Prefix": (61, 2, (8, 0), (), "Prefix", None),
		"ReplyStyle": (101, 2, (3, 0), (), "ReplyStyle", None),
		"ResponseStyle": (102, 2, (3, 0), (), "ResponseStyle", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"ShowOn": (105, 2, (3, 0), (), "ShowOn", None),
	}
		_prop_map_put_ = {
		"CopyLike": ((100, LCID, 4, 0),()),
		"Enabled": ((103, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Name": ((12289, LCID, 4, 0),()),
		"Prefix": ((61, LCID, 4, 0),()),
		"ReplyStyle": ((101, LCID, 4, 0),()),
		"ResponseStyle": ((102, LCID, 4, 0),()),
		"ShowOn": ((105, LCID, 4, 0),()),
	}
class  Actions (DispatchBaseClass) :
	CLSID = IID('{0006303E-0000-0000-C000-000000000046}')
		coclass_clsid = None
		
	def Add(self):

		ret = self._oleobj_.InvokeTypes(100, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Add', '{00063043-0000-0000-C000-000000000046}')

		return ret
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', '{00063043-0000-0000-C000-000000000046}')

		return ret
 def Remove(self, Index=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(82, LCID, 1, (24, 0), ((3, 1),),Index
			)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  AddressEntries (DispatchBaseClass) :
	CLSID = IID('{0006304A-0000-0000-C000-000000000046}')
		coclass_clsid = None
		
	def Add(self, Type=defaultNamedNotOptArg, Name=defaultNamedOptArg, Address=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(95, LCID, 1, (9, 0), ((8, 1), (12, 17), (12, 17)),Type
			, Name, Address)

		if ret is not None:

			ret = Dispatch(ret, 'Add', '{0006304B-0000-0000-C000-000000000046}')

		return ret
 def GetFirst(self):

		ret = self._oleobj_.InvokeTypes(86, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetFirst', '{0006304B-0000-0000-C000-000000000046}')

		return ret
 def GetLast(self):

		ret = self._oleobj_.InvokeTypes(88, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetLast', '{0006304B-0000-0000-C000-000000000046}')

		return ret
 def GetNext(self):

		ret = self._oleobj_.InvokeTypes(87, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetNext', '{0006304B-0000-0000-C000-000000000046}')

		return ret
 def GetPrevious(self):

		ret = self._oleobj_.InvokeTypes(89, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetPrevious', '{0006304B-0000-0000-C000-000000000046}')

		return ret
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', '{0006304B-0000-0000-C000-000000000046}')

		return ret
 def Sort(self, Property=defaultNamedOptArg, Order=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(97, LCID, 1, (24, 0), ((12, 17), (12, 17)),Property
			, Order)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"RawTable": (90, 2, (13, 0), (), "RawTable", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  AddressEntry (DispatchBaseClass) :
	CLSID = IID('{0006304B-0000-0000-C000-000000000046}')
		coclass_clsid = None
		def Delete(self):

		return self._oleobj_.InvokeTypes(770, LCID, 1, (24, 0), (),)
 def Details(self, HWnd=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(769, LCID, 1, (24, 0), ((12, 17),),HWnd
			)
 def GetFreeBusy(self, Start=defaultNamedNotOptArg, MinPerChar=defaultNamedNotOptArg, CompleteFormat=defaultNamedOptArg):

		
		return self._oleobj_.InvokeTypes(774, LCID, 1, (8, 0), ((7, 1), (3, 1), (12, 17)),Start
			, MinPerChar, CompleteFormat)
 def Update(self, MakePermanent=defaultNamedOptArg, Refresh=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(768, LCID, 1, (24, 0), ((12, 17), (12, 17)),MakePermanent
			, Refresh)
 def UpdateFreeBusy(self):

		return self._oleobj_.InvokeTypes(775, LCID, 1, (24, 0), (),)

	_prop_map_get_ = {
		"Address": (12291, 2, (8, 0), (), "Address", None),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"DisplayType": (14592, 2, (3, 0), (), "DisplayType", None),
		"ID": (61470, 2, (8, 0), (), "ID", None),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		
		"Manager": (771, 2, (9, 0), (), "Manager", '{0006304B-0000-0000-C000-000000000046}'),
		
		"Members": (772, 2, (9, 0), (), "Members", '{0006304A-0000-0000-C000-000000000046}'),
		"Name": (12289, 2, (8, 0), (), "Name", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Type": (12290, 2, (8, 0), (), "Type", None),
	}
		_prop_map_put_ = {
		"Address": ((12291, LCID, 4, 0),()),
		"MAPIOBJECT": ((61696, LCID, 4, 0),()),
		"Name": ((12289, LCID, 4, 0),()),
		"Type": ((12290, LCID, 4, 0),()),
	}
class  AddressList (DispatchBaseClass) :
	CLSID = IID('{00063049-0000-0000-C000-000000000046}')
		coclass_clsid = None
		_prop_map_get_ = {
		
		"AddressEntries": (256, 2, (9, 0), (), "AddressEntries", '{0006304A-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"ID": (61470, 2, (8, 0), (), "ID", None),
		"Index": (91, 2, (3, 0), (), "Index", None),
		"IsReadOnly": (61463, 2, (11, 0), (), "IsReadOnly", None),
		"Name": (12289, 2, (8, 0), (), "Name", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
class  AddressLists (DispatchBaseClass) :
	CLSID = IID('{00063048-0000-0000-C000-000000000046}')
		coclass_clsid = None
		
	def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', '{00063049-0000-0000-C000-000000000046}')

		return ret

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  ApplicationEvents :
	CLSID = CLSID_Sink = IID('{0006304E-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{0006F03A-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		    61447 : "OnQuit",
		    61442 : "OnItemSend",
		    61446 : "OnStartup",
		    61443 : "OnNewMail",
		    61444 : "OnReminder",
		    61445 : "OnOptionsPagesAdd",
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  Attachment (DispatchBaseClass) :
	CLSID = IID('{00063007-0000-0000-C000-000000000046}')
		coclass_clsid = None
		def Delete(self):

		return self._oleobj_.InvokeTypes(105, LCID, 1, (24, 0), (),)
 def SaveAsFile(self, Path=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(104, LCID, 1, (24, 0), ((8, 1),),Path
			)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"DisplayName": (12289, 2, (8, 0), (), "DisplayName", None),
		"FileName": (14084, 2, (8, 0), (), "FileName", None),
		"Index": (91, 2, (3, 0), (), "Index", None),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"Parent": (113, 2, (9, 0), (), "Parent", None),
		"PathName": (14088, 2, (8, 0), (), "PathName", None),
		"Position": (114, 2, (3, 0), (), "Position", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Type": (14085, 2, (3, 0), (), "Type", None),
	}
		_prop_map_put_ = {
		"DisplayName": ((12289, LCID, 4, 0),()),
		"Position": ((114, LCID, 4, 0),()),
	}
class  Attachments (DispatchBaseClass) :
	CLSID = IID('{0006303C-0000-0000-C000-000000000046}')
		coclass_clsid = None
		
	def Add(self, Source=defaultNamedNotOptArg, Type=defaultNamedOptArg, Position=defaultNamedOptArg, DisplayName=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(101, LCID, 1, (9, 0), ((12, 1), (12, 17), (12, 17), (12, 17)),Source
			, Type, Position, DisplayName)

		if ret is not None:

			ret = Dispatch(ret, 'Add', '{00063007-0000-0000-C000-000000000046}')

		return ret
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', '{00063007-0000-0000-C000-000000000046}')

		return ret
 def Remove(self, Index=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(84, LCID, 1, (24, 0), ((3, 1),),Index
			)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  Exception (DispatchBaseClass) :
	CLSID = IID('{0006304D-0000-0000-C000-000000000046}')
		coclass_clsid = None
		_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"AppointmentItem": (8193, 2, (13, 0), (), "AppointmentItem", '{00061030-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Deleted": (8194, 2, (11, 0), (), "Deleted", None),
		"OriginalDate": (8192, 2, (7, 0), (), "OriginalDate", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
class  Exceptions (DispatchBaseClass) :
	CLSID = IID('{0006304C-0000-0000-C000-000000000046}')
		coclass_clsid = None
		
	def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', '{0006304D-0000-0000-C000-000000000046}')

		return ret

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  ExplorerEvents :
	CLSID = CLSID_Sink = IID('{0006304F-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063050-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		    61445 : "OnBeforeViewSwitch",
		    61441 : "OnActivate",
		    61444 : "OnViewSwitch",
		    61446 : "OnDeactivate",
		    61447 : "OnSelectionChange",
		    61443 : "OnBeforeFolderSwitch",
		    61448 : "OnClose",
		    61442 : "OnFolderSwitch",
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  ExplorersEvents :
	CLSID = CLSID_Sink = IID('{00063078-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063053-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		    61441 : "OnNewExplorer",
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  FoldersEvents :
	CLSID = CLSID_Sink = IID('{00063076-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063051-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		    61442 : "OnFolderChange",
		    61443 : "OnFolderRemove",
		    61441 : "OnFolderAdd",
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  FormDescription (DispatchBaseClass) :
	CLSID = IID('{00063046-0000-0000-C000-000000000046}')
		coclass_clsid = None
		def PublishForm(self, Registry=defaultNamedNotOptArg, Folder=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(107, LCID, 1, (24, 0), ((3, 1), (12, 17)),Registry
			, Folder)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Category": (13060, 2, (8, 0), (), "Category", None),
		"CategorySub": (13061, 2, (8, 0), (), "CategorySub", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Comment": (12292, 2, (8, 0), (), "Comment", None),
		"ContactName": (13059, 2, (8, 0), (), "ContactName", None),
		"DisplayName": (12289, 2, (8, 0), (), "DisplayName", None),
		"Hidden": (13063, 2, (11, 0), (), "Hidden", None),
		"Icon": (4093, 2, (8, 0), (), "Icon", None),
		"Locked": (102, 2, (11, 0), (), "Locked", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"MiniIcon": (4092, 2, (8, 0), (), "MiniIcon", None),
		"Name": (61469, 2, (8, 0), (), "Name", None),
		"Number": (104, 2, (8, 0), (), "Number", None),
		"OneOff": (101, 2, (11, 0), (), "OneOff", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"Password": (103, 2, (8, 0), (), "Password", None),
		"ScriptText": (109, 2, (8, 0), (), "ScriptText", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Template": (106, 2, (8, 0), (), "Template", None),
		"UseWordMail": (105, 2, (11, 0), (), "UseWordMail", None),
		"Version": (13057, 2, (8, 0), (), "Version", None),
	}
		_prop_map_put_ = {
		"Category": ((13060, LCID, 4, 0),()),
		"CategorySub": ((13061, LCID, 4, 0),()),
		"Comment": ((12292, LCID, 4, 0),()),
		"ContactName": ((13059, LCID, 4, 0),()),
		"DisplayName": ((12289, LCID, 4, 0),()),
		"Hidden": ((13063, LCID, 4, 0),()),
		"Icon": ((4093, LCID, 4, 0),()),
		"Locked": ((102, LCID, 4, 0),()),
		"MiniIcon": ((4092, LCID, 4, 0),()),
		"Name": ((61469, LCID, 4, 0),()),
		"Number": ((104, LCID, 4, 0),()),
		"OneOff": ((101, LCID, 4, 0),()),
		"Password": ((103, LCID, 4, 0),()),
		"Template": ((106, LCID, 4, 0),()),
		"UseWordMail": ((105, LCID, 4, 0),()),
		"Version": ((13057, LCID, 4, 0),()),
	}
class  InspectorEvents :
	CLSID = CLSID_Sink = IID('{0006307D-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063058-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		    61446 : "OnDeactivate",
		    61448 : "OnClose",
		    61441 : "OnActivate",
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  InspectorsEvents :
	CLSID = CLSID_Sink = IID('{00063079-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063054-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		    61441 : "OnNewInspector",
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  ItemEvents :
	CLSID = CLSID_Sink = IID('{0006303A-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061051-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		    61452 : "OnAttachmentRead",
		    61451 : "OnAttachmentAdd",
		    61448 : "OnCustomPropertyChange",
		    61441 : "OnRead",
		    61449 : "OnPropertyChange",
		    61445 : "OnSend",
		    61450 : "OnBeforeCheckNames",
		    61442 : "OnWrite",
		    61453 : "OnBeforeAttachmentSave",
		    62566 : "OnReply",
		    62568 : "OnForward",
		    61444 : "OnClose",
		    61443 : "OnOpen",
		    62567 : "OnReplyAll",
		    61446 : "OnCustomAction",
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  ItemsEvents :
	CLSID = CLSID_Sink = IID('{00063077-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063052-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		    61441 : "OnItemAdd",
		    61442 : "OnItemChange",
		    61443 : "OnItemRemove",
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  Link (DispatchBaseClass) :
	CLSID = IID('{00063089-0000-0000-C000-000000000046}')
		coclass_clsid = None
		_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Item": (8450, 2, (9, 0), (), "Item", None),
		"Name": (12289, 2, (8, 0), (), "Name", None),
		"Parent": (109, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Type": (8449, 2, (3, 0), (), "Type", None),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(8450, LCID, 2, 1, item)), "Item")

class  Links (DispatchBaseClass) :
	CLSID = IID('{0006308A-0000-0000-C000-000000000046}')
		coclass_clsid = None
		
	def Add(self, Item=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(95, LCID, 1, (9, 0), ((9, 1),),Item
			)

		if ret is not None:

			ret = Dispatch(ret, 'Add', '{00063089-0000-0000-C000-000000000046}')

		return ret
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', '{00063089-0000-0000-C000-000000000046}')

		return ret
 def Remove(self, Index=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(84, LCID, 1, (24, 0), ((12, 1),),Index
			)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  MAPIFolder (DispatchBaseClass) :
	CLSID = IID('{00063006-0000-0000-C000-000000000046}')
		coclass_clsid = None
		def AddToPFFavorites(self):

		return self._oleobj_.InvokeTypes(12565, LCID, 1, (24, 0), (),)
 def CopyTo(self, DestinationFolder=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), ((9, 1),),DestinationFolder
			)

		if ret is not None:

			ret = Dispatch(ret, 'CopyTo', '{00063006-0000-0000-C000-000000000046}')

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61509, LCID, 1, (24, 0), (),)
 def Display(self):

		return self._oleobj_.InvokeTypes(12548, LCID, 1, (24, 0), (),)
 def GetExplorer(self, DisplayMode=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(12545, LCID, 1, (9, 0), ((12, 17),),DisplayMode
			)

		if ret is not None:

			ret = Dispatch(ret, 'GetExplorer', '{00063003-0000-0000-C000-000000000046}')

		return ret
 def MoveTo(self, DestinationFolder=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61492, LCID, 1, (24, 0), ((9, 1),),DestinationFolder
			)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"DefaultItemType": (12550, 2, (3, 0), (), "DefaultItemType", None),
		"DefaultMessageClass": (12551, 2, (8, 0), (), "DefaultMessageClass", None),
		"Description": (12292, 2, (8, 0), (), "Description", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		
		"Folders": (8451, 2, (9, 0), (), "Folders", '{00063040-0000-0000-C000-000000000046}'),
		
		"Items": (12544, 2, (9, 0), (), "Items", '{00063041-0000-0000-C000-000000000046}'),
		"Name": (12289, 2, (8, 0), (), "Name", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"StoreID": (12552, 2, (8, 0), (), "StoreID", None),
		"UnReadItemCount": (13827, 2, (3, 0), (), "UnReadItemCount", None),
		"UserPermissions": (12561, 2, (9, 0), (), "UserPermissions", None),
		"WebViewAllowNavigation": (12564, 2, (11, 0), (), "WebViewAllowNavigation", None),
		"WebViewOn": (12562, 2, (11, 0), (), "WebViewOn", None),
		"WebViewURL": (12563, 2, (8, 0), (), "WebViewURL", None),
	}
		_prop_map_put_ = {
		"Description": ((12292, LCID, 4, 0),()),
		"Name": ((12289, LCID, 4, 0),()),
		"WebViewAllowNavigation": ((12564, LCID, 4, 0),()),
		"WebViewOn": ((12562, LCID, 4, 0),()),
		"WebViewURL": ((12563, LCID, 4, 0),()),
	}
class  NameSpaceEvents :
	CLSID = CLSID_Sink = IID('{0006308C-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{0006308B-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		    61445 : "OnOptionsPagesAdd",
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  OutlookBarGroup (DispatchBaseClass) :
	CLSID = IID('{00063073-0000-0000-C000-000000000046}')
		coclass_clsid = None
		_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Name": (0, 2, (8, 0), (), "Name", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		
		"Shortcuts": (8450, 2, (9, 0), (), "Shortcuts", '{00063074-0000-0000-C000-000000000046}'),
		"ViewType": (8451, 2, (3, 0), (), "ViewType", None),
	}
		_prop_map_put_ = {
		"Name": ((0, LCID, 4, 0),()),
		"ViewType": ((8451, LCID, 4, 0),()),
	}
		
	def __call__(self):

		return self._ApplyTypes_(*(0, 2, (8, 0), (), "Name", None))
 def __unicode__(self, *args):

		try:

			return str(self.__call__(*args))

		except pythoncom.com_error:

			return repr(self)
 def __str__(self, *args):

		return str(self.__unicode__(*args))
 def __int__(self, *args):

		return int(self.__call__(*args))

class  OutlookBarGroupsEvents :
	CLSID = CLSID_Sink = IID('{0006307B-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063056-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		    61441 : "OnGroupAdd",
		    61443 : "OnBeforeGroupRemove",
		    61442 : "OnBeforeGroupAdd",
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  OutlookBarPaneEvents :
	CLSID = CLSID_Sink = IID('{0006307A-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063055-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		    61441 : "OnBeforeNavigate",
		    61442 : "OnBeforeGroupSwitch",
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  OutlookBarShortcut (DispatchBaseClass) :
	CLSID = IID('{00063075-0000-0000-C000-000000000046}')
		coclass_clsid = None
		_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Name": (0, 2, (8, 0), (), "Name", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Target": (8448, 2, (12, 0), (), "Target", None),
	}
		_prop_map_put_ = {
		"Name": ((0, LCID, 4, 0),()),
	}
		
	def __call__(self):

		return self._ApplyTypes_(*(0, 2, (8, 0), (), "Name", None))
 def __unicode__(self, *args):

		try:

			return str(self.__call__(*args))

		except pythoncom.com_error:

			return repr(self)
 def __str__(self, *args):

		return str(self.__unicode__(*args))
 def __int__(self, *args):

		return int(self.__call__(*args))

class  OutlookBarShortcutsEvents :
	CLSID = CLSID_Sink = IID('{0006307C-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063057-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		    61441 : "OnShortcutAdd",
		    61442 : "OnBeforeShortcutAdd",
		    61443 : "OnBeforeShortcutRemove",
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  OutlookBarStorage (DispatchBaseClass) :
	CLSID = IID('{00063071-0000-0000-C000-000000000046}')
		coclass_clsid = None
		_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		
		"Groups": (0, 2, (9, 0), (), "Groups", '{00063072-0000-0000-C000-000000000046}'),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __call__(self):

		return self._ApplyTypes_(*(0, 2, (9, 0), (), "Groups", '{00063072-0000-0000-C000-000000000046}'))
 def __unicode__(self, *args):

		try:

			return str(self.__call__(*args))

		except pythoncom.com_error:

			return repr(self)
 def __str__(self, *args):

		return str(self.__unicode__(*args))
 def __int__(self, *args):

		return int(self.__call__(*args))

class  Pages (DispatchBaseClass) :
	CLSID = IID('{0006303F-0000-0000-C000-000000000046}')
		coclass_clsid = None
		def Add(self, Name=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(300, LCID, 1, (9, 0), ((12, 17),),Name
			)

		if ret is not None:

			ret = Dispatch(ret, 'Add', None)

		return ret
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', None)

		return ret
 def Remove(self, Index=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(301, LCID, 1, (24, 0), ((3, 1),),Index
			)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  Panes (DispatchBaseClass) :
	CLSID = IID('{00063009-0000-0000-C000-000000000046}')
		coclass_clsid = None
		def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', None)

		return ret

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  PropertyPage (DispatchBaseClass) :
	CLSID = IID('{0006307E-0000-0000-C000-000000000046}')
		coclass_clsid = None
		_prop_map_get_ = {
		"Dirty": (8449, 2, (3, 0), ((16395, 10),), "Dirty", None),
	}
		_prop_map_put_ = {
	}
class  PropertyPageSite (DispatchBaseClass) :
	CLSID = IID('{0006307F-0000-0000-C000-000000000046}')
		coclass_clsid = None
		def OnStatusChange(self):

		return self._oleobj_.InvokeTypes(8448, LCID, 1, (24, 0), (),)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
class  PropertyPages (DispatchBaseClass) :
	CLSID = IID('{00063080-0000-0000-C000-000000000046}')
		coclass_clsid = None
		def Add(self, Page=defaultNamedNotOptArg, Title=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(95, LCID, 1, (24, 0), ((12, 1), (8, 17)),Page
			, Title)
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', None)

		return ret
 def Remove(self, Index=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(84, LCID, 1, (24, 0), ((12, 1),),Index
			)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  Recipient (DispatchBaseClass) :
	CLSID = IID('{00063045-0000-0000-C000-000000000046}')
		coclass_clsid = None
		def Delete(self):

		return self._oleobj_.InvokeTypes(110, LCID, 1, (24, 0), (),)
 def FreeBusy(self, Start=defaultNamedNotOptArg, MinPerChar=defaultNamedNotOptArg, CompleteFormat=defaultNamedOptArg):

		
		return self._oleobj_.InvokeTypes(111, LCID, 1, (8, 0), ((7, 1), (3, 1), (12, 17)),Start
			, MinPerChar, CompleteFormat)
 def Resolve(self):

		return self._oleobj_.InvokeTypes(113, LCID, 1, (11, 0), (),)

	_prop_map_get_ = {
		"Address": (12291, 2, (8, 0), (), "Address", None),
		
		"AddressEntry": (121, 2, (9, 0), (), "AddressEntry", '{0006304B-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"AutoResponse": (106, 2, (8, 0), (), "AutoResponse", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"DisplayType": (14592, 2, (3, 0), (), "DisplayType", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		"Index": (91, 2, (3, 0), (), "Index", None),
		"MeetingResponseStatus": (102, 2, (3, 0), (), "MeetingResponseStatus", None),
		"Name": (12289, 2, (8, 0), (), "Name", None),
		"Parent": (109, 2, (9, 0), (), "Parent", None),
		"Resolved": (100, 2, (11, 0), (), "Resolved", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"TrackingStatus": (118, 2, (3, 0), (), "TrackingStatus", None),
		"TrackingStatusTime": (119, 2, (7, 0), (), "TrackingStatusTime", None),
		"Type": (3093, 2, (3, 0), (), "Type", None),
	}
		_prop_map_put_ = {
		"AddressEntry": ((121, LCID, 8, 0),()),
		"AutoResponse": ((106, LCID, 4, 0),()),
		"TrackingStatus": ((118, LCID, 4, 0),()),
		"TrackingStatusTime": ((119, LCID, 4, 0),()),
		"Type": ((3093, LCID, 4, 0),()),
	}
class  Recipients (DispatchBaseClass) :
	CLSID = IID('{0006303B-0000-0000-C000-000000000046}')
		coclass_clsid = None
		
	def Add(self, Name=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(111, LCID, 1, (9, 0), ((8, 1),),Name
			)

		if ret is not None:

			ret = Dispatch(ret, 'Add', '{00063045-0000-0000-C000-000000000046}')

		return ret
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', '{00063045-0000-0000-C000-000000000046}')

		return ret
 def Remove(self, Index=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(84, LCID, 1, (24, 0), ((3, 1),),Index
			)
 def ResolveAll(self):

		return self._oleobj_.InvokeTypes(126, LCID, 1, (11, 0), (),)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  RecurrencePattern (DispatchBaseClass) :
	CLSID = IID('{00063044-0000-0000-C000-000000000046}')
		coclass_clsid = None
		
	def GetOccurrence(self, StartDate=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(4111, LCID, 1, (13, 0), ((7, 1),),StartDate
			)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'GetOccurrence', '{00061030-0000-0000-C000-000000000046}')

		return ret

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"DayOfMonth": (4096, 2, (3, 0), (), "DayOfMonth", None),
		"DayOfWeekMask": (4097, 2, (3, 0), (), "DayOfWeekMask", None),
		"Duration": (4109, 2, (3, 0), (), "Duration", None),
		"EndTime": (4108, 2, (7, 0), (), "EndTime", None),
		
		"Exceptions": (4110, 2, (9, 0), (), "Exceptions", '{0006304C-0000-0000-C000-000000000046}'),
		"Instance": (4099, 2, (3, 0), (), "Instance", None),
		"Interval": (4100, 2, (3, 0), (), "Interval", None),
		"MonthOfYear": (4102, 2, (3, 0), (), "MonthOfYear", None),
		"NoEndDate": (4107, 2, (11, 0), (), "NoEndDate", None),
		"Occurrences": (4101, 2, (3, 0), (), "Occurrences", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"PatternEndDate": (4098, 2, (7, 0), (), "PatternEndDate", None),
		"PatternStartDate": (4104, 2, (7, 0), (), "PatternStartDate", None),
		"RecurrenceType": (4103, 2, (3, 0), (), "RecurrenceType", None),
		"Regenerate": (4106, 2, (11, 0), (), "Regenerate", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"StartTime": (4105, 2, (7, 0), (), "StartTime", None),
	}
		_prop_map_put_ = {
		"DayOfMonth": ((4096, LCID, 4, 0),()),
		"DayOfWeekMask": ((4097, LCID, 4, 0),()),
		"Duration": ((4109, LCID, 4, 0),()),
		"EndTime": ((4108, LCID, 4, 0),()),
		"Instance": ((4099, LCID, 4, 0),()),
		"Interval": ((4100, LCID, 4, 0),()),
		"MonthOfYear": ((4102, LCID, 4, 0),()),
		"NoEndDate": ((4107, LCID, 4, 0),()),
		"Occurrences": ((4101, LCID, 4, 0),()),
		"PatternEndDate": ((4098, LCID, 4, 0),()),
		"PatternStartDate": ((4104, LCID, 4, 0),()),
		"RecurrenceType": ((4103, LCID, 4, 0),()),
		"Regenerate": ((4106, LCID, 4, 0),()),
		"StartTime": ((4105, LCID, 4, 0),()),
	}
class  Selection (DispatchBaseClass) :
	CLSID = IID('{00063087-0000-0000-C000-000000000046}')
		coclass_clsid = None
		def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', None)

		return ret

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  SyncObjectEvents :
	CLSID = CLSID_Sink = IID('{00063085-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063084-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		    61443 : "OnError",
		    61442 : "OnProgress",
		    61441 : "OnSyncStart",
		    61444 : "OnSyncEnd",
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  SyncObjects (DispatchBaseClass) :
	CLSID = IID('{00063086-0000-0000-C000-000000000046}')
		coclass_clsid = None
		
	def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (13, 0), ((12, 1),),Index
			)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Item', '{00063084-0000-0000-C000-000000000046}')

		return ret

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  UserProperties (DispatchBaseClass) :
	CLSID = IID('{0006303D-0000-0000-C000-000000000046}')
		coclass_clsid = None
		
	def Add(self, Name=defaultNamedNotOptArg, Type=defaultNamedNotOptArg, AddToFolderFields=defaultNamedOptArg, DisplayFormat=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(102, LCID, 1, (9, 0), ((8, 1), (3, 1), (12, 17), (12, 17)),Name
			, Type, AddToFolderFields, DisplayFormat)

		if ret is not None:

			ret = Dispatch(ret, 'Add', '{00063042-0000-0000-C000-000000000046}')

		return ret
 def Find(self, Name=defaultNamedNotOptArg, Custom=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(103, LCID, 1, (9, 0), ((8, 1), (12, 17)),Name
			, Custom)

		if ret is not None:

			ret = Dispatch(ret, 'Find', '{00063042-0000-0000-C000-000000000046}')

		return ret
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', '{00063042-0000-0000-C000-000000000046}')

		return ret
 def Remove(self, Index=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(82, LCID, 1, (24, 0), ((3, 1),),Index
			)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  UserProperty (DispatchBaseClass) :
	CLSID = IID('{00063042-0000-0000-C000-000000000046}')
		coclass_clsid = None
		def Delete(self):

		return self._oleobj_.InvokeTypes(108, LCID, 1, (24, 0), (),)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Formula": (103, 2, (8, 0), (), "Formula", None),
		"Name": (112, 2, (8, 0), (), "Name", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Type": (109, 2, (3, 0), (), "Type", None),
		"ValidationFormula": (104, 2, (8, 0), (), "ValidationFormula", None),
		"ValidationText": (105, 2, (8, 0), (), "ValidationText", None),
		"Value": (0, 2, (12, 0), (), "Value", None),
	}
		_prop_map_put_ = {
		"Formula": ((103, LCID, 4, 0),()),
		"ValidationFormula": ((104, LCID, 4, 0),()),
		"ValidationText": ((105, LCID, 4, 0),()),
		"Value": ((0, LCID, 4, 0),()),
	}
		
	def __call__(self):

		return self._ApplyTypes_(*(0, 2, (12, 0), (), "Value", None))
 def __unicode__(self, *args):

		try:

			return str(self.__call__(*args))

		except pythoncom.com_error:

			return repr(self)
 def __str__(self, *args):

		return str(self.__unicode__(*args))
 def __int__(self, *args):

		return int(self.__call__(*args))

class  _Application (DispatchBaseClass) :
	CLSID = IID('{00063001-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{0006F03A-0000-0000-C000-000000000046}')
		
	def ActiveExplorer(self):

		ret = self._oleobj_.InvokeTypes(273, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'ActiveExplorer', '{00063003-0000-0000-C000-000000000046}')

		return ret
 def ActiveInspector(self):

		ret = self._oleobj_.InvokeTypes(274, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'ActiveInspector', '{00063005-0000-0000-C000-000000000046}')

		return ret
 def ActiveWindow(self):

		ret = self._oleobj_.InvokeTypes(287, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'ActiveWindow', None)

		return ret
 def CreateItem(self, ItemType=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(266, LCID, 1, (9, 0), ((3, 1),),ItemType
			)

		if ret is not None:

			ret = Dispatch(ret, 'CreateItem', None)

		return ret
 def CreateItemFromTemplate(self, TemplatePath=defaultNamedNotOptArg, InFolder=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(267, LCID, 1, (9, 0), ((8, 1), (12, 17)),TemplatePath
			, InFolder)

		if ret is not None:

			ret = Dispatch(ret, 'CreateItemFromTemplate', None)

		return ret
 def CreateObject(self, ObjectName=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(277, LCID, 1, (9, 0), ((8, 1),),ObjectName
			)

		if ret is not None:

			ret = Dispatch(ret, 'CreateObject', None)

		return ret
 def GetNamespace(self, Type=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(272, LCID, 1, (9, 0), ((8, 1),),Type
			)

		if ret is not None:

			ret = Dispatch(ret, 'GetNamespace', '{00063002-0000-0000-C000-000000000046}')

		return ret
 def Quit(self):

		return self._oleobj_.InvokeTypes(275, LCID, 1, (24, 0), (),)

	_prop_map_get_ = {
		
		"AnswerWizard": (285, 2, (9, 0), (), "AnswerWizard", '{000C0360-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Assistant": (276, 2, (9, 0), (), "Assistant", '{000C0322-0000-0000-C000-000000000046}'),
		
		"COMAddIns": (280, 2, (9, 0), (), "COMAddIns", '{000C0339-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		
		"Explorers": (281, 2, (9, 0), (), "Explorers", '{0006300A-0000-0000-C000-000000000046}'),
		"FeatureInstall": (286, 2, (3, 0), (), "FeatureInstall", None),
		
		"Inspectors": (282, 2, (9, 0), (), "Inspectors", '{00063008-0000-0000-C000-000000000046}'),
		
		"LanguageSettings": (283, 2, (9, 0), (), "LanguageSettings", '{000C0353-0000-0000-C000-000000000046}'),
		"Name": (12289, 2, (8, 0), (), "Name", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"ProductCode": (284, 2, (8, 0), (), "ProductCode", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Version": (278, 2, (8, 0), (), "Version", None),
	}
		_prop_map_put_ = {
		"FeatureInstall": ((286, LCID, 4, 0),()),
	}
class  _AppointmentItem (DispatchBaseClass) :
	CLSID = IID('{00063033-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061030-0000-0000-C000-000000000046}')
		def ClearRecurrencePattern(self):

		return self._oleobj_.InvokeTypes(61605, LCID, 1, (24, 0), (),)
 def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def ForwardAsVcal(self):

		ret = self._oleobj_.InvokeTypes(62791, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'ForwardAsVcal', '{00061033-0000-0000-C000-000000000046}')

		return ret
 def GetRecurrencePattern(self):

		ret = self._oleobj_.InvokeTypes(61604, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetRecurrencePattern', '{00063044-0000-0000-C000-000000000046}')

		return ret
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Respond(self, Response=defaultNamedNotOptArg, fNoUI=defaultNamedOptArg, fAdditionalTextDialog=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(62722, LCID, 1, (13, 0), ((3, 1), (12, 17), (12, 17)),Response
			, fNoUI, fAdditionalTextDialog)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Respond', '{00061036-0000-0000-C000-000000000046}')

		return ret
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)
 def Send(self):

		return self._oleobj_.InvokeTypes(61557, LCID, 1, (24, 0), (),)

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		"AllDayEvent": (33301, 2, (11, 0), (), "AllDayEvent", None),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"BusyStatus": (33285, 2, (3, 0), (), "BusyStatus", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"ConferenceServerAllowExternal": (33350, 2, (11, 0), (), "ConferenceServerAllowExternal", None),
		"ConferenceServerPassword": (33353, 2, (8, 0), (), "ConferenceServerPassword", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"Duration": (33299, 2, (3, 0), (), "Duration", None),
		"End": (33294, 2, (7, 0), (), "End", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"IsOnlineMeeting": (33344, 2, (11, 0), (), "IsOnlineMeeting", None),
		"IsRecurring": (33315, 2, (11, 0), (), "IsRecurring", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"Location": (33288, 2, (8, 0), (), "Location", None),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MeetingStatus": (33303, 2, (3, 0), (), "MeetingStatus", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NetMeetingAutoStart": (33348, 2, (11, 0), (), "NetMeetingAutoStart", None),
		"NetMeetingDocPathName": (33351, 2, (8, 0), (), "NetMeetingDocPathName", None),
		"NetMeetingOrganizerAlias": (33347, 2, (8, 0), (), "NetMeetingOrganizerAlias", None),
		"NetMeetingServer": (33346, 2, (8, 0), (), "NetMeetingServer", None),
		"NetMeetingType": (33345, 2, (3, 0), (), "NetMeetingType", None),
		"NetShowURL": (33352, 2, (8, 0), (), "NetShowURL", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OptionalAttendees": (3587, 2, (8, 0), (), "OptionalAttendees", None),
		"Organizer": (66, 2, (8, 0), (), "Organizer", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Recipients": (63508, 2, (9, 0), (), "Recipients", '{0006303B-0000-0000-C000-000000000046}'),
		"RecurrenceState": (62789, 2, (3, 0), (), "RecurrenceState", None),
		"ReminderMinutesBeforeStart": (34049, 2, (3, 0), (), "ReminderMinutesBeforeStart", None),
		"ReminderOverrideDefault": (34076, 2, (11, 0), (), "ReminderOverrideDefault", None),
		"ReminderPlaySound": (34078, 2, (11, 0), (), "ReminderPlaySound", None),
		"ReminderSet": (34051, 2, (11, 0), (), "ReminderSet", None),
		"ReminderSoundFile": (34079, 2, (8, 0), (), "ReminderSoundFile", None),
		"ReplyTime": (33312, 2, (7, 0), (), "ReplyTime", None),
		"RequiredAttendees": (3588, 2, (8, 0), (), "RequiredAttendees", None),
		"Resources": (3586, 2, (8, 0), (), "Resources", None),
		"ResponseRequested": (99, 2, (11, 0), (), "ResponseRequested", None),
		"ResponseStatus": (33304, 2, (3, 0), (), "ResponseStatus", None),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Start": (33293, 2, (7, 0), (), "Start", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"AllDayEvent": ((33301, LCID, 4, 0),()),
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"BusyStatus": ((33285, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"ConferenceServerAllowExternal": ((33350, LCID, 4, 0),()),
		"ConferenceServerPassword": ((33353, LCID, 4, 0),()),
		"Duration": ((33299, LCID, 4, 0),()),
		"End": ((33294, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"IsOnlineMeeting": ((33344, LCID, 4, 0),()),
		"Location": ((33288, LCID, 4, 0),()),
		"MeetingStatus": ((33303, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NetMeetingAutoStart": ((33348, LCID, 4, 0),()),
		"NetMeetingDocPathName": ((33351, LCID, 4, 0),()),
		"NetMeetingOrganizerAlias": ((33347, LCID, 4, 0),()),
		"NetMeetingServer": ((33346, LCID, 4, 0),()),
		"NetMeetingType": ((33345, LCID, 4, 0),()),
		"NetShowURL": ((33352, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"OptionalAttendees": ((3587, LCID, 4, 0),()),
		"ReminderMinutesBeforeStart": ((34049, LCID, 4, 0),()),
		"ReminderOverrideDefault": ((34076, LCID, 4, 0),()),
		"ReminderPlaySound": ((34078, LCID, 4, 0),()),
		"ReminderSet": ((34051, LCID, 4, 0),()),
		"ReminderSoundFile": ((34079, LCID, 4, 0),()),
		"ReplyTime": ((33312, LCID, 4, 0),()),
		"RequiredAttendees": ((3588, LCID, 4, 0),()),
		"Resources": ((3586, LCID, 4, 0),()),
		"ResponseRequested": ((99, LCID, 4, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"Start": ((33293, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
	}
class  _ContactItem (DispatchBaseClass) :
	CLSID = IID('{00063021-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061031-0000-0000-C000-000000000046}')
		def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def ForwardAsVcard(self):

		ret = self._oleobj_.InvokeTypes(63649, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'ForwardAsVcard', '{00061033-0000-0000-C000-000000000046}')

		return ret
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)

	_prop_map_get_ = {
		"Account": (14848, 2, (8, 0), (), "Account", None),
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		"Anniversary": (14913, 2, (7, 0), (), "Anniversary", None),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"AssistantName": (14896, 2, (8, 0), (), "AssistantName", None),
		"AssistantTelephoneNumber": (14894, 2, (8, 0), (), "AssistantTelephoneNumber", None),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Birthday": (14914, 2, (7, 0), (), "Birthday", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"Business2TelephoneNumber": (14875, 2, (8, 0), (), "Business2TelephoneNumber", None),
		"BusinessAddress": (32795, 2, (8, 0), (), "BusinessAddress", None),
		"BusinessAddressCity": (32838, 2, (8, 0), (), "BusinessAddressCity", None),
		"BusinessAddressCountry": (32841, 2, (8, 0), (), "BusinessAddressCountry", None),
		"BusinessAddressPostOfficeBox": (32842, 2, (8, 0), (), "BusinessAddressPostOfficeBox", None),
		"BusinessAddressPostalCode": (32840, 2, (8, 0), (), "BusinessAddressPostalCode", None),
		"BusinessAddressState": (32839, 2, (8, 0), (), "BusinessAddressState", None),
		"BusinessAddressStreet": (32837, 2, (8, 0), (), "BusinessAddressStreet", None),
		"BusinessFaxNumber": (14884, 2, (8, 0), (), "BusinessFaxNumber", None),
		"BusinessHomePage": (14929, 2, (8, 0), (), "BusinessHomePage", None),
		"BusinessTelephoneNumber": (14856, 2, (8, 0), (), "BusinessTelephoneNumber", None),
		"CallbackTelephoneNumber": (14850, 2, (8, 0), (), "CallbackTelephoneNumber", None),
		"CarTelephoneNumber": (14878, 2, (8, 0), (), "CarTelephoneNumber", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Children": (32780, 2, (8, 0), (), "Children", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"CompanyAndFullName": (32792, 2, (8, 0), (), "CompanyAndFullName", None),
		"CompanyLastFirstNoSpace": (32818, 2, (8, 0), (), "CompanyLastFirstNoSpace", None),
		"CompanyLastFirstSpaceOnly": (32819, 2, (8, 0), (), "CompanyLastFirstSpaceOnly", None),
		"CompanyMainTelephoneNumber": (14935, 2, (8, 0), (), "CompanyMainTelephoneNumber", None),
		"CompanyName": (14870, 2, (8, 0), (), "CompanyName", None),
		"ComputerNetworkName": (14921, 2, (8, 0), (), "ComputerNetworkName", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"CustomerID": (14922, 2, (8, 0), (), "CustomerID", None),
		"Department": (14872, 2, (8, 0), (), "Department", None),
		"Email1Address": (32899, 2, (8, 0), (), "Email1Address", None),
		"Email1AddressType": (32898, 2, (8, 0), (), "Email1AddressType", None),
		"Email1DisplayName": (32896, 2, (8, 0), (), "Email1DisplayName", None),
		"Email1EntryID": (32901, 2, (8, 0), (), "Email1EntryID", None),
		"Email2Address": (32915, 2, (8, 0), (), "Email2Address", None),
		"Email2AddressType": (32914, 2, (8, 0), (), "Email2AddressType", None),
		"Email2DisplayName": (32912, 2, (8, 0), (), "Email2DisplayName", None),
		"Email2EntryID": (32917, 2, (8, 0), (), "Email2EntryID", None),
		"Email3Address": (32931, 2, (8, 0), (), "Email3Address", None),
		"Email3AddressType": (32930, 2, (8, 0), (), "Email3AddressType", None),
		"Email3DisplayName": (32928, 2, (8, 0), (), "Email3DisplayName", None),
		"Email3EntryID": (32933, 2, (8, 0), (), "Email3EntryID", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		"FTPSite": (14924, 2, (8, 0), (), "FTPSite", None),
		"FileAs": (32773, 2, (8, 0), (), "FileAs", None),
		"FirstName": (14854, 2, (8, 0), (), "FirstName", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		"FullName": (12289, 2, (8, 0), (), "FullName", None),
		"FullNameAndCompany": (32793, 2, (8, 0), (), "FullNameAndCompany", None),
		"Gender": (14925, 2, (3, 0), (), "Gender", None),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"GovernmentIDNumber": (14855, 2, (8, 0), (), "GovernmentIDNumber", None),
		"Hobby": (14915, 2, (8, 0), (), "Hobby", None),
		"Home2TelephoneNumber": (14895, 2, (8, 0), (), "Home2TelephoneNumber", None),
		"HomeAddress": (32794, 2, (8, 0), (), "HomeAddress", None),
		"HomeAddressCity": (14937, 2, (8, 0), (), "HomeAddressCity", None),
		"HomeAddressCountry": (14938, 2, (8, 0), (), "HomeAddressCountry", None),
		"HomeAddressPostOfficeBox": (14942, 2, (8, 0), (), "HomeAddressPostOfficeBox", None),
		"HomeAddressPostalCode": (14939, 2, (8, 0), (), "HomeAddressPostalCode", None),
		"HomeAddressState": (14940, 2, (8, 0), (), "HomeAddressState", None),
		"HomeAddressStreet": (14941, 2, (8, 0), (), "HomeAddressStreet", None),
		"HomeFaxNumber": (14885, 2, (8, 0), (), "HomeFaxNumber", None),
		"HomeTelephoneNumber": (14857, 2, (8, 0), (), "HomeTelephoneNumber", None),
		"ISDNNumber": (14893, 2, (8, 0), (), "ISDNNumber", None),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"Initials": (14858, 2, (8, 0), (), "Initials", None),
		"InternetFreeBusyAddress": (32984, 2, (8, 0), (), "InternetFreeBusyAddress", None),
		"JobTitle": (14871, 2, (8, 0), (), "JobTitle", None),
		"Journal": (32805, 2, (11, 0), (), "Journal", None),
		"Language": (14860, 2, (8, 0), (), "Language", None),
		"LastFirstAndSuffix": (32822, 2, (8, 0), (), "LastFirstAndSuffix", None),
		"LastFirstNoSpace": (32816, 2, (8, 0), (), "LastFirstNoSpace", None),
		"LastFirstNoSpaceCompany": (32820, 2, (8, 0), (), "LastFirstNoSpaceCompany", None),
		"LastFirstSpaceOnly": (32817, 2, (8, 0), (), "LastFirstSpaceOnly", None),
		"LastFirstSpaceOnlyCompany": (32821, 2, (8, 0), (), "LastFirstSpaceOnlyCompany", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		"LastName": (14865, 2, (8, 0), (), "LastName", None),
		"LastNameAndFirstName": (32791, 2, (8, 0), (), "LastNameAndFirstName", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MailingAddress": (14869, 2, (8, 0), (), "MailingAddress", None),
		"MailingAddressCity": (14887, 2, (8, 0), (), "MailingAddressCity", None),
		"MailingAddressCountry": (14886, 2, (8, 0), (), "MailingAddressCountry", None),
		"MailingAddressPostOfficeBox": (14891, 2, (8, 0), (), "MailingAddressPostOfficeBox", None),
		"MailingAddressPostalCode": (14890, 2, (8, 0), (), "MailingAddressPostalCode", None),
		"MailingAddressState": (14888, 2, (8, 0), (), "MailingAddressState", None),
		"MailingAddressStreet": (14889, 2, (8, 0), (), "MailingAddressStreet", None),
		"ManagerName": (14926, 2, (8, 0), (), "ManagerName", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"MiddleName": (14916, 2, (8, 0), (), "MiddleName", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"MobileTelephoneNumber": (14876, 2, (8, 0), (), "MobileTelephoneNumber", None),
		"NetMeetingAlias": (32863, 2, (8, 0), (), "NetMeetingAlias", None),
		"NetMeetingServer": (32864, 2, (8, 0), (), "NetMeetingServer", None),
		"NickName": (14927, 2, (8, 0), (), "NickName", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OfficeLocation": (14873, 2, (8, 0), (), "OfficeLocation", None),
		"OrganizationalIDNumber": (14864, 2, (8, 0), (), "OrganizationalIDNumber", None),
		"OtherAddress": (32796, 2, (8, 0), (), "OtherAddress", None),
		"OtherAddressCity": (14943, 2, (8, 0), (), "OtherAddressCity", None),
		"OtherAddressCountry": (14944, 2, (8, 0), (), "OtherAddressCountry", None),
		"OtherAddressPostOfficeBox": (14948, 2, (8, 0), (), "OtherAddressPostOfficeBox", None),
		"OtherAddressPostalCode": (14945, 2, (8, 0), (), "OtherAddressPostalCode", None),
		"OtherAddressState": (14946, 2, (8, 0), (), "OtherAddressState", None),
		"OtherAddressStreet": (14947, 2, (8, 0), (), "OtherAddressStreet", None),
		"OtherFaxNumber": (14883, 2, (8, 0), (), "OtherFaxNumber", None),
		"OtherTelephoneNumber": (14879, 2, (8, 0), (), "OtherTelephoneNumber", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"PagerNumber": (14881, 2, (8, 0), (), "PagerNumber", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"PersonalHomePage": (14928, 2, (8, 0), (), "PersonalHomePage", None),
		"PrimaryTelephoneNumber": (14874, 2, (8, 0), (), "PrimaryTelephoneNumber", None),
		"Profession": (14918, 2, (8, 0), (), "Profession", None),
		"RadioTelephoneNumber": (14877, 2, (8, 0), (), "RadioTelephoneNumber", None),
		"ReferredBy": (14919, 2, (8, 0), (), "ReferredBy", None),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"SelectedMailingAddress": (32802, 2, (3, 0), (), "SelectedMailingAddress", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Spouse": (14920, 2, (8, 0), (), "Spouse", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"Suffix": (14853, 2, (8, 0), (), "Suffix", None),
		"TTYTDDTelephoneNumber": (14923, 2, (8, 0), (), "TTYTDDTelephoneNumber", None),
		"TelexNumber": (14892, 2, (8, 0), (), "TelexNumber", None),
		"Title": (14917, 2, (8, 0), (), "Title", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		"User1": (32847, 2, (8, 0), (), "User1", None),
		"User2": (32848, 2, (8, 0), (), "User2", None),
		"User3": (32849, 2, (8, 0), (), "User3", None),
		"User4": (32850, 2, (8, 0), (), "User4", None),
		"UserCertificate": (32790, 2, (8, 0), (), "UserCertificate", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
		"WebPage": (32811, 2, (8, 0), (), "WebPage", None),
		"YomiCompanyName": (32814, 2, (8, 0), (), "YomiCompanyName", None),
		"YomiFirstName": (32812, 2, (8, 0), (), "YomiFirstName", None),
		"YomiLastName": (32813, 2, (8, 0), (), "YomiLastName", None),
	}
		_prop_map_put_ = {
		"Account": ((14848, LCID, 4, 0),()),
		"Anniversary": ((14913, LCID, 4, 0),()),
		"AssistantName": ((14896, LCID, 4, 0),()),
		"AssistantTelephoneNumber": ((14894, LCID, 4, 0),()),
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Birthday": ((14914, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"Business2TelephoneNumber": ((14875, LCID, 4, 0),()),
		"BusinessAddress": ((32795, LCID, 4, 0),()),
		"BusinessAddressCity": ((32838, LCID, 4, 0),()),
		"BusinessAddressCountry": ((32841, LCID, 4, 0),()),
		"BusinessAddressPostOfficeBox": ((32842, LCID, 4, 0),()),
		"BusinessAddressPostalCode": ((32840, LCID, 4, 0),()),
		"BusinessAddressState": ((32839, LCID, 4, 0),()),
		"BusinessAddressStreet": ((32837, LCID, 4, 0),()),
		"BusinessFaxNumber": ((14884, LCID, 4, 0),()),
		"BusinessHomePage": ((14929, LCID, 4, 0),()),
		"BusinessTelephoneNumber": ((14856, LCID, 4, 0),()),
		"CallbackTelephoneNumber": ((14850, LCID, 4, 0),()),
		"CarTelephoneNumber": ((14878, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Children": ((32780, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"CompanyMainTelephoneNumber": ((14935, LCID, 4, 0),()),
		"CompanyName": ((14870, LCID, 4, 0),()),
		"ComputerNetworkName": ((14921, LCID, 4, 0),()),
		"CustomerID": ((14922, LCID, 4, 0),()),
		"Department": ((14872, LCID, 4, 0),()),
		"Email1Address": ((32899, LCID, 4, 0),()),
		"Email1AddressType": ((32898, LCID, 4, 0),()),
		"Email2Address": ((32915, LCID, 4, 0),()),
		"Email2AddressType": ((32914, LCID, 4, 0),()),
		"Email3Address": ((32931, LCID, 4, 0),()),
		"Email3AddressType": ((32930, LCID, 4, 0),()),
		"FTPSite": ((14924, LCID, 4, 0),()),
		"FileAs": ((32773, LCID, 4, 0),()),
		"FirstName": ((14854, LCID, 4, 0),()),
		"FullName": ((12289, LCID, 4, 0),()),
		"Gender": ((14925, LCID, 4, 0),()),
		"GovernmentIDNumber": ((14855, LCID, 4, 0),()),
		"Hobby": ((14915, LCID, 4, 0),()),
		"Home2TelephoneNumber": ((14895, LCID, 4, 0),()),
		"HomeAddress": ((32794, LCID, 4, 0),()),
		"HomeAddressCity": ((14937, LCID, 4, 0),()),
		"HomeAddressCountry": ((14938, LCID, 4, 0),()),
		"HomeAddressPostOfficeBox": ((14942, LCID, 4, 0),()),
		"HomeAddressPostalCode": ((14939, LCID, 4, 0),()),
		"HomeAddressState": ((14940, LCID, 4, 0),()),
		"HomeAddressStreet": ((14941, LCID, 4, 0),()),
		"HomeFaxNumber": ((14885, LCID, 4, 0),()),
		"HomeTelephoneNumber": ((14857, LCID, 4, 0),()),
		"ISDNNumber": ((14893, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"Initials": ((14858, LCID, 4, 0),()),
		"InternetFreeBusyAddress": ((32984, LCID, 4, 0),()),
		"JobTitle": ((14871, LCID, 4, 0),()),
		"Journal": ((32805, LCID, 4, 0),()),
		"Language": ((14860, LCID, 4, 0),()),
		"LastName": ((14865, LCID, 4, 0),()),
		"MailingAddress": ((14869, LCID, 4, 0),()),
		"MailingAddressCity": ((14887, LCID, 4, 0),()),
		"MailingAddressCountry": ((14886, LCID, 4, 0),()),
		"MailingAddressPostOfficeBox": ((14891, LCID, 4, 0),()),
		"MailingAddressPostalCode": ((14890, LCID, 4, 0),()),
		"MailingAddressState": ((14888, LCID, 4, 0),()),
		"MailingAddressStreet": ((14889, LCID, 4, 0),()),
		"ManagerName": ((14926, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"MiddleName": ((14916, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"MobileTelephoneNumber": ((14876, LCID, 4, 0),()),
		"NetMeetingAlias": ((32863, LCID, 4, 0),()),
		"NetMeetingServer": ((32864, LCID, 4, 0),()),
		"NickName": ((14927, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"OfficeLocation": ((14873, LCID, 4, 0),()),
		"OrganizationalIDNumber": ((14864, LCID, 4, 0),()),
		"OtherAddress": ((32796, LCID, 4, 0),()),
		"OtherAddressCity": ((14943, LCID, 4, 0),()),
		"OtherAddressCountry": ((14944, LCID, 4, 0),()),
		"OtherAddressPostOfficeBox": ((14948, LCID, 4, 0),()),
		"OtherAddressPostalCode": ((14945, LCID, 4, 0),()),
		"OtherAddressState": ((14946, LCID, 4, 0),()),
		"OtherAddressStreet": ((14947, LCID, 4, 0),()),
		"OtherFaxNumber": ((14883, LCID, 4, 0),()),
		"OtherTelephoneNumber": ((14879, LCID, 4, 0),()),
		"PagerNumber": ((14881, LCID, 4, 0),()),
		"PersonalHomePage": ((14928, LCID, 4, 0),()),
		"PrimaryTelephoneNumber": ((14874, LCID, 4, 0),()),
		"Profession": ((14918, LCID, 4, 0),()),
		"RadioTelephoneNumber": ((14877, LCID, 4, 0),()),
		"ReferredBy": ((14919, LCID, 4, 0),()),
		"SelectedMailingAddress": ((32802, LCID, 4, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"Spouse": ((14920, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"Suffix": ((14853, LCID, 4, 0),()),
		"TTYTDDTelephoneNumber": ((14923, LCID, 4, 0),()),
		"TelexNumber": ((14892, LCID, 4, 0),()),
		"Title": ((14917, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
		"User1": ((32847, LCID, 4, 0),()),
		"User2": ((32848, LCID, 4, 0),()),
		"User3": ((32849, LCID, 4, 0),()),
		"User4": ((32850, LCID, 4, 0),()),
		"UserCertificate": ((32790, LCID, 4, 0),()),
		"WebPage": ((32811, LCID, 4, 0),()),
		"YomiCompanyName": ((32814, LCID, 4, 0),()),
		"YomiFirstName": ((32812, LCID, 4, 0),()),
		"YomiLastName": ((32813, LCID, 4, 0),()),
	}
class  _DDocSiteControl (DispatchBaseClass) :
	CLSID = IID('{0006F026-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{0006F024-0000-0000-C000-000000000046}')
		_prop_map_get_ = {
		"ReadOnly": (-2147356664, 2, (11, 0), (), "ReadOnly", None),
	}
		_prop_map_put_ = {
		"ReadOnly": ((-2147356664, LCID, 4, 0),()),
	}
class  _DDocSiteControlEvents :
	CLSID = CLSID_Sink = IID('{50BB9B50-811D-11CE-B565-00AA00608FAA}')
		coclass_clsid = IID('{0006F024-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  _DRecipientControl (DispatchBaseClass) :
	CLSID = IID('{0006F025-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{0006F023-0000-0000-C000-000000000046}')
		_prop_map_get_ = {
		"BackColor": (-501, 2, (3, 0), (), "BackColor", None),
		"Enabled": (-514, 2, (11, 0), (), "Enabled", None),
		"Font": (-512, 2, (9, 0), (), "Font", None),
		"ForeColor": (-513, 2, (3, 0), (), "ForeColor", None),
		"ReadOnly": (-2147356664, 2, (11, 0), (), "ReadOnly", None),
		"SpecialEffect": (12, 2, (3, 0), (), "SpecialEffect", None),
	}
		_prop_map_put_ = {
		"BackColor": ((-501, LCID, 4, 0),()),
		"Enabled": ((-514, LCID, 4, 0),()),
		"Font": ((-512, LCID, 4, 0),()),
		"ForeColor": ((-513, LCID, 4, 0),()),
		"ReadOnly": ((-2147356664, LCID, 4, 0),()),
		"SpecialEffect": ((12, LCID, 4, 0),()),
	}
class  _DRecipientControlEvents :
	CLSID = CLSID_Sink = IID('{D87E7E17-6897-11CE-A6C0-00AA00608FAA}')
		coclass_clsid = IID('{0006F023-0000-0000-C000-000000000046}')
		_public_methods_ = []
		_dispid_to_func_ = {
		}
		def __init__(self, oobj = None):

		if oobj is None:

			self._olecp = None

		else:

			import win32com.server.util

			from win32com.server.policy import EventHandlerPolicy

			cpc=oobj._oleobj_.QueryInterface(pythoncom.IID_IConnectionPointContainer)

			cp=cpc.FindConnectionPoint(self.CLSID_Sink)

			cookie=cp.Advise(win32com.server.util.wrap(self, usePolicy=EventHandlerPolicy))

			self._olecp,self._olecp_cookie = cp,cookie
 def __del__(self):

		try:

			self.close()

		except pythoncom.com_error:

			pass
 def close(self):

		if self._olecp is not None:

			cp,cookie,self._olecp,self._olecp_cookie = self._olecp,self._olecp_cookie,None,None

			cp.Unadvise(cookie)
 def _query_interface_(self, iid):

		import win32com.server.util

		if iid==self.CLSID_Sink: return win32com.server.util.wrap(self)

class  _DistListItem (DispatchBaseClass) :
	CLSID = IID('{00063081-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{0006103C-0000-0000-C000-000000000046}')
		def AddMembers(self, Recipients=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(63744, LCID, 1, (24, 0), ((9, 1),),Recipients
			)
 def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def GetMember(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(63749, LCID, 1, (9, 0), ((3, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'GetMember', '{00063045-0000-0000-C000-000000000046}')

		return ret
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def RemoveMembers(self, Recipients=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(63745, LCID, 1, (24, 0), ((9, 1),),Recipients
			)
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"CheckSum": (32844, 2, (3, 0), (), "CheckSum", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"DLName": (32851, 2, (8, 0), (), "DLName", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MemberCount": (32843, 2, (3, 0), (), "MemberCount", None),
		"Members": (32853, 2, (12, 0), (), "Members", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OneOffMembers": (32852, 2, (12, 0), (), "OneOffMembers", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"DLName": ((32851, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"Members": ((32853, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"OneOffMembers": ((32852, LCID, 4, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
	}
class  _DocumentItem (DispatchBaseClass) :
	CLSID = IID('{00063020-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061061-0000-0000-C000-000000000046}')
		def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
	}
class  _Explorer (DispatchBaseClass) :
	CLSID = IID('{00063003-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063050-0000-0000-C000-000000000046}')
		def Activate(self):

		return self._oleobj_.InvokeTypes(8467, LCID, 1, (24, 0), (),)
 def Close(self):

		return self._oleobj_.InvokeTypes(8451, LCID, 1, (24, 0), (),)
 def Display(self):

		return self._oleobj_.InvokeTypes(8452, LCID, 1, (24, 0), (),)
 def IsPaneVisible(self, Pane=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(8707, LCID, 1, (11, 0), ((3, 1),),Pane
			)
 def ShowPane(self, Pane=defaultNamedNotOptArg, Visible=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(8708, LCID, 1, (24, 0), ((3, 1), (11, 1)),Pane
			, Visible)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Caption": (8465, 2, (8, 0), (), "Caption", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		
		"CommandBars": (8448, 2, (13, 0), (), "CommandBars", '{55F88893-7708-11D1-ACEB-006008961DA5}'),
		
		"CurrentFolder": (8449, 2, (9, 0), (), "CurrentFolder", '{00063006-0000-0000-C000-000000000046}'),
		"CurrentView": (8704, 2, (12, 0), (), "CurrentView", None),
		"Height": (8468, 2, (3, 0), (), "Height", None),
		"Left": (8469, 2, (3, 0), (), "Left", None),
		
		"Panes": (8705, 2, (9, 0), (), "Panes", '{00063009-0000-0000-C000-000000000046}'),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Selection": (8706, 2, (9, 0), (), "Selection", '{00063087-0000-0000-C000-000000000046}'),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Top": (8470, 2, (3, 0), (), "Top", None),
		"Views": (12553, 2, (9, 0), (), "Views", None),
		"Width": (8471, 2, (3, 0), (), "Width", None),
		"WindowState": (8466, 2, (3, 0), (), "WindowState", None),
	}
		_prop_map_put_ = {
		"CurrentFolder": ((8449, LCID, 8, 0),()),
		"CurrentView": ((8704, LCID, 4, 0),()),
		"Height": ((8468, LCID, 4, 0),()),
		"Left": ((8469, LCID, 4, 0),()),
		"Top": ((8470, LCID, 4, 0),()),
		"Width": ((8471, LCID, 4, 0),()),
		"WindowState": ((8466, LCID, 4, 0),()),
	}
class  _Explorers (DispatchBaseClass) :
	CLSID = IID('{0006300A-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063053-0000-0000-C000-000000000046}')
		
	def Add(self, Folder=defaultNamedNotOptArg, DisplayMode=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(95, LCID, 1, (9, 0), ((12, 1), (3, 17)),Folder
			, DisplayMode)

		if ret is not None:

			ret = Dispatch(ret, 'Add', '{00063003-0000-0000-C000-000000000046}')

		return ret
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (13, 0), ((12, 1),),Index
			)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Item', '{00063050-0000-0000-C000-000000000046}')

		return ret

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  _Folders (DispatchBaseClass) :
	CLSID = IID('{00063040-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063051-0000-0000-C000-000000000046}')
		
	def Add(self, Name=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(95, LCID, 1, (9, 0), ((8, 1), (12, 17)),Name
			, Type)

		if ret is not None:

			ret = Dispatch(ret, 'Add', '{00063006-0000-0000-C000-000000000046}')

		return ret
 def GetFirst(self):

		ret = self._oleobj_.InvokeTypes(86, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetFirst', '{00063006-0000-0000-C000-000000000046}')

		return ret
 def GetLast(self):

		ret = self._oleobj_.InvokeTypes(88, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetLast', '{00063006-0000-0000-C000-000000000046}')

		return ret
 def GetNext(self):

		ret = self._oleobj_.InvokeTypes(87, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetNext', '{00063006-0000-0000-C000-000000000046}')

		return ret
 def GetPrevious(self):

		ret = self._oleobj_.InvokeTypes(89, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetPrevious', '{00063006-0000-0000-C000-000000000046}')

		return ret
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', '{00063006-0000-0000-C000-000000000046}')

		return ret
 def Remove(self, Index=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(84, LCID, 1, (24, 0), ((3, 1),),Index
			)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"RawTable": (90, 2, (13, 0), (), "RawTable", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  _IDocSiteControl (DispatchBaseClass) :
	CLSID = IID('{43507DD0-811D-11CE-B565-00AA00608FAA}')
		coclass_clsid = None
		_prop_map_get_ = {
		"ReadOnly": (-2147356664, 2, (3, 0), ((16395, 10),), "ReadOnly", None),
	}
		_prop_map_put_ = {
		"ReadOnly": ((-2147356664, LCID, 4, 0),()),
	}
class  _IRecipientControl (DispatchBaseClass) :
	CLSID = IID('{D87E7E16-6897-11CE-A6C0-00AA00608FAA}')
		coclass_clsid = None
		_prop_map_get_ = {
		"BackColor": (-501, 2, (3, 0), ((16387, 10),), "BackColor", None),
		"Enabled": (-514, 2, (3, 0), ((16395, 10),), "Enabled", None),
		"Font": (-512, 2, (3, 0), ((16393, 10),), "Font", None),
		"ForeColor": (-513, 2, (3, 0), ((16387, 10),), "ForeColor", None),
		"ReadOnly": (-2147356664, 2, (3, 0), ((16395, 10),), "ReadOnly", None),
		"SpecialEffect": (12, 2, (3, 0), ((16387, 10),), "SpecialEffect", None),
	}
		_prop_map_put_ = {
		"BackColor": ((-501, LCID, 4, 0),()),
		"Enabled": ((-514, LCID, 4, 0),()),
		"Font": ((-512, LCID, 4, 0),()),
		"ForeColor": ((-513, LCID, 4, 0),()),
		"ReadOnly": ((-2147356664, LCID, 4, 0),()),
		"SpecialEffect": ((12, LCID, 4, 0),()),
	}
class  _Inspector (DispatchBaseClass) :
	CLSID = IID('{00063005-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063058-0000-0000-C000-000000000046}')
		def Activate(self):

		return self._oleobj_.InvokeTypes(8467, LCID, 1, (24, 0), (),)
 def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(8451, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(8452, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def HideFormPage(self, PageName=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(8456, LCID, 1, (24, 0), ((8, 1),),PageName
			)
 def IsWordMail(self):

		return self._oleobj_.InvokeTypes(8453, LCID, 1, (11, 0), (),)
 def SetCurrentFormPage(self, PageName=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(8460, LCID, 1, (24, 0), ((8, 1),),PageName
			)
 def ShowFormPage(self, PageName=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(8457, LCID, 1, (24, 0), ((8, 1),),PageName
			)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Caption": (8465, 2, (8, 0), (), "Caption", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		
		"CommandBars": (8448, 2, (13, 0), (), "CommandBars", '{55F88893-7708-11D1-ACEB-006008961DA5}'),
		"CurrentItem": (8450, 2, (9, 0), (), "CurrentItem", None),
		"EditorType": (8464, 2, (3, 0), (), "EditorType", None),
		"HTMLEditor": (8462, 2, (9, 0), (), "HTMLEditor", None),
		"Height": (8468, 2, (3, 0), (), "Height", None),
		"Left": (8469, 2, (3, 0), (), "Left", None),
		"ModifiedFormPages": (8454, 2, (9, 0), (), "ModifiedFormPages", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Top": (8470, 2, (3, 0), (), "Top", None),
		"Width": (8471, 2, (3, 0), (), "Width", None),
		"WindowState": (8466, 2, (3, 0), (), "WindowState", None),
		"WordEditor": (8463, 2, (9, 0), (), "WordEditor", None),
	}
		_prop_map_put_ = {
		"Height": ((8468, LCID, 4, 0),()),
		"Left": ((8469, LCID, 4, 0),()),
		"Top": ((8470, LCID, 4, 0),()),
		"Width": ((8471, LCID, 4, 0),()),
		"WindowState": ((8466, LCID, 4, 0),()),
	}
class  _Inspectors (DispatchBaseClass) :
	CLSID = IID('{00063008-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063054-0000-0000-C000-000000000046}')
		
	def Add(self, Item=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(95, LCID, 1, (9, 0), ((9, 1),),Item
			)

		if ret is not None:

			ret = Dispatch(ret, 'Add', '{00063005-0000-0000-C000-000000000046}')

		return ret
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (13, 0), ((12, 1),),Index
			)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Item', '{00063058-0000-0000-C000-000000000046}')

		return ret

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  _Items (DispatchBaseClass) :
	CLSID = IID('{00063041-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063052-0000-0000-C000-000000000046}')
		def Add(self, Type=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(95, LCID, 1, (9, 0), ((12, 17),),Type
			)

		if ret is not None:

			ret = Dispatch(ret, 'Add', None)

		return ret
 def Find(self, Filter=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(98, LCID, 1, (9, 0), ((8, 1),),Filter
			)

		if ret is not None:

			ret = Dispatch(ret, 'Find', None)

		return ret
 def FindNext(self):

		ret = self._oleobj_.InvokeTypes(99, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'FindNext', None)

		return ret
 def GetFirst(self):

		ret = self._oleobj_.InvokeTypes(86, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetFirst', None)

		return ret
 def GetLast(self):

		ret = self._oleobj_.InvokeTypes(88, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetLast', None)

		return ret
 def GetNext(self):

		ret = self._oleobj_.InvokeTypes(87, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetNext', None)

		return ret
 def GetPrevious(self):

		ret = self._oleobj_.InvokeTypes(89, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetPrevious', None)

		return ret
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', None)

		return ret
 def Remove(self, Index=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(84, LCID, 1, (24, 0), ((3, 1),),Index
			)
 def ResetColumns(self):

		return self._oleobj_.InvokeTypes(93, LCID, 1, (24, 0), (),)
 def Restrict(self, Filter=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(100, LCID, 1, (9, 0), ((8, 1),),Filter
			)

		if ret is not None:

			ret = Dispatch(ret, 'Restrict', '{00063041-0000-0000-C000-000000000046}')

		return ret
 def SetColumns(self, Columns=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(92, LCID, 1, (24, 0), ((8, 1),),Columns
			)
 def Sort(self, Property=defaultNamedNotOptArg, Descending=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(97, LCID, 1, (24, 0), ((8, 1), (12, 17)),Property
			, Descending)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"IncludeRecurrences": (206, 2, (11, 0), (), "IncludeRecurrences", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"RawTable": (90, 2, (13, 0), (), "RawTable", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"IncludeRecurrences": ((206, LCID, 4, 0),()),
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  _JournalItem (DispatchBaseClass) :
	CLSID = IID('{00063022-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061037-0000-0000-C000-000000000046}')
		def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def Forward(self):

		ret = self._oleobj_.InvokeTypes(63507, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Forward', '{00061033-0000-0000-C000-000000000046}')

		return ret
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Reply(self):

		ret = self._oleobj_.InvokeTypes(63504, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Reply', '{00061033-0000-0000-C000-000000000046}')

		return ret
 def ReplyAll(self):

		ret = self._oleobj_.InvokeTypes(63505, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'ReplyAll', '{00061033-0000-0000-C000-000000000046}')

		return ret
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)
 def StartTimer(self):

		return self._oleobj_.InvokeTypes(63269, LCID, 1, (24, 0), (),)
 def StopTimer(self):

		return self._oleobj_.InvokeTypes(63270, LCID, 1, (24, 0), (),)

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"ContactNames": (3588, 2, (8, 0), (), "ContactNames", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"DocPosted": (34577, 2, (11, 0), (), "DocPosted", None),
		"DocPrinted": (34574, 2, (11, 0), (), "DocPrinted", None),
		"DocRouted": (34576, 2, (11, 0), (), "DocRouted", None),
		"DocSaved": (34575, 2, (11, 0), (), "DocSaved", None),
		"Duration": (34567, 2, (3, 0), (), "Duration", None),
		"End": (34568, 2, (7, 0), (), "End", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Recipients": (63508, 2, (9, 0), (), "Recipients", '{0006303B-0000-0000-C000-000000000046}'),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Start": (34566, 2, (7, 0), (), "Start", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"Type": (34560, 2, (8, 0), (), "Type", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"ContactNames": ((3588, LCID, 4, 0),()),
		"DocPosted": ((34577, LCID, 4, 0),()),
		"DocPrinted": ((34574, LCID, 4, 0),()),
		"DocRouted": ((34576, LCID, 4, 0),()),
		"DocSaved": ((34575, LCID, 4, 0),()),
		"Duration": ((34567, LCID, 4, 0),()),
		"End": ((34568, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"Start": ((34566, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"Type": ((34560, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
	}
class  _MailItem (DispatchBaseClass) :
	CLSID = IID('{00063034-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061033-0000-0000-C000-000000000046}')
		def ClearConversationIndex(self):

		return self._oleobj_.InvokeTypes(63522, LCID, 1, (24, 0), (),)
 def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def Forward(self):

		ret = self._oleobj_.InvokeTypes(63507, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Forward', '{00061033-0000-0000-C000-000000000046}')

		return ret
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Reply(self):

		ret = self._oleobj_.InvokeTypes(63504, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Reply', '{00061033-0000-0000-C000-000000000046}')

		return ret
 def ReplyAll(self):

		ret = self._oleobj_.InvokeTypes(63505, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'ReplyAll', '{00061033-0000-0000-C000-000000000046}')

		return ret
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)
 def Send(self):

		return self._oleobj_.InvokeTypes(61557, LCID, 1, (24, 0), (),)

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		"AlternateRecipientAllowed": (2, 2, (11, 0), (), "AlternateRecipientAllowed", None),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"AutoForwarded": (5, 2, (11, 0), (), "AutoForwarded", None),
		"BCC": (3586, 2, (8, 0), (), "BCC", None),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"CC": (3587, 2, (8, 0), (), "CC", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"DeferredDeliveryTime": (15, 2, (7, 0), (), "DeferredDeliveryTime", None),
		"DeleteAfterSubmit": (3585, 2, (11, 0), (), "DeleteAfterSubmit", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		"ExpiryTime": (21, 2, (7, 0), (), "ExpiryTime", None),
		"FlagDueBy": (48, 2, (7, 0), (), "FlagDueBy", None),
		"FlagRequest": (34096, 2, (8, 0), (), "FlagRequest", None),
		"FlagStatus": (4240, 2, (3, 0), (), "FlagStatus", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"HTMLBody": (62468, 2, (8, 0), (), "HTMLBody", None),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OriginatorDeliveryReportRequested": (35, 2, (11, 0), (), "OriginatorDeliveryReportRequested", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"ReadReceiptRequested": (41, 2, (11, 0), (), "ReadReceiptRequested", None),
		"ReceivedByEntryID": (63, 2, (8, 0), (), "ReceivedByEntryID", None),
		"ReceivedByName": (64, 2, (8, 0), (), "ReceivedByName", None),
		"ReceivedOnBehalfOfEntryID": (67, 2, (8, 0), (), "ReceivedOnBehalfOfEntryID", None),
		"ReceivedOnBehalfOfName": (68, 2, (8, 0), (), "ReceivedOnBehalfOfName", None),
		"ReceivedTime": (3590, 2, (7, 0), (), "ReceivedTime", None),
		"RecipientReassignmentProhibited": (43, 2, (11, 0), (), "RecipientReassignmentProhibited", None),
		
		"Recipients": (63508, 2, (9, 0), (), "Recipients", '{0006303B-0000-0000-C000-000000000046}'),
		"ReminderOverrideDefault": (34076, 2, (11, 0), (), "ReminderOverrideDefault", None),
		"ReminderPlaySound": (34078, 2, (11, 0), (), "ReminderPlaySound", None),
		"ReminderSet": (34051, 2, (11, 0), (), "ReminderSet", None),
		"ReminderSoundFile": (34079, 2, (8, 0), (), "ReminderSoundFile", None),
		"ReminderTime": (34050, 2, (7, 0), (), "ReminderTime", None),
		"RemoteStatus": (34065, 2, (3, 0), (), "RemoteStatus", None),
		"ReplyRecipientNames": (80, 2, (8, 0), (), "ReplyRecipientNames", None),
		
		"ReplyRecipients": (61459, 2, (9, 0), (), "ReplyRecipients", '{0006303B-0000-0000-C000-000000000046}'),
		
		"SaveSentMessageFolder": (62465, 2, (9, 0), (), "SaveSentMessageFolder", '{00063006-0000-0000-C000-000000000046}'),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"SenderName": (3098, 2, (8, 0), (), "SenderName", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		"Sent": (62466, 2, (11, 0), (), "Sent", None),
		"SentOn": (57, 2, (7, 0), (), "SentOn", None),
		"SentOnBehalfOfName": (66, 2, (8, 0), (), "SentOnBehalfOfName", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"Submitted": (62467, 2, (11, 0), (), "Submitted", None),
		"To": (3588, 2, (8, 0), (), "To", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
		"VotingOptions": (61467, 2, (8, 0), (), "VotingOptions", None),
		"VotingResponse": (34084, 2, (8, 0), (), "VotingResponse", None),
	}
		_prop_map_put_ = {
		"AlternateRecipientAllowed": ((2, LCID, 4, 0),()),
		"AutoForwarded": ((5, LCID, 4, 0),()),
		"BCC": ((3586, LCID, 4, 0),()),
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"CC": ((3587, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"DeferredDeliveryTime": ((15, LCID, 4, 0),()),
		"DeleteAfterSubmit": ((3585, LCID, 4, 0),()),
		"ExpiryTime": ((21, LCID, 4, 0),()),
		"FlagDueBy": ((48, LCID, 4, 0),()),
		"FlagRequest": ((34096, LCID, 4, 0),()),
		"FlagStatus": ((4240, LCID, 4, 0),()),
		"HTMLBody": ((62468, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"OriginatorDeliveryReportRequested": ((35, LCID, 4, 0),()),
		"ReadReceiptRequested": ((41, LCID, 4, 0),()),
		"RecipientReassignmentProhibited": ((43, LCID, 4, 0),()),
		"ReminderOverrideDefault": ((34076, LCID, 4, 0),()),
		"ReminderPlaySound": ((34078, LCID, 4, 0),()),
		"ReminderSet": ((34051, LCID, 4, 0),()),
		"ReminderSoundFile": ((34079, LCID, 4, 0),()),
		"ReminderTime": ((34050, LCID, 4, 0),()),
		"RemoteStatus": ((34065, LCID, 4, 0),()),
		"SaveSentMessageFolder": ((62465, LCID, 8, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"SentOnBehalfOfName": ((66, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"To": ((3588, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
		"VotingOptions": ((61467, LCID, 4, 0),()),
		"VotingResponse": ((34084, LCID, 4, 0),()),
	}
class  _MeetingItem (DispatchBaseClass) :
	CLSID = IID('{00063062-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061036-0000-0000-C000-000000000046}')
		def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def Forward(self):

		ret = self._oleobj_.InvokeTypes(63507, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Forward', '{00061036-0000-0000-C000-000000000046}')

		return ret
 def GetAssociatedAppointment(self, AddToCalendar=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(63328, LCID, 1, (13, 0), ((11, 1),),AddToCalendar
			)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'GetAssociatedAppointment', '{00061030-0000-0000-C000-000000000046}')

		return ret
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Reply(self):

		ret = self._oleobj_.InvokeTypes(63504, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Reply', '{00061033-0000-0000-C000-000000000046}')

		return ret
 def ReplyAll(self):

		ret = self._oleobj_.InvokeTypes(63505, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'ReplyAll', '{00061033-0000-0000-C000-000000000046}')

		return ret
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)
 def Send(self):

		return self._oleobj_.InvokeTypes(61557, LCID, 1, (24, 0), (),)

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"AutoForwarded": (5, 2, (11, 0), (), "AutoForwarded", None),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"DeferredDeliveryTime": (15, 2, (7, 0), (), "DeferredDeliveryTime", None),
		"DeleteAfterSubmit": (3585, 2, (11, 0), (), "DeleteAfterSubmit", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		"ExpiryTime": (21, 2, (7, 0), (), "ExpiryTime", None),
		"FlagDueBy": (48, 2, (7, 0), (), "FlagDueBy", None),
		"FlagRequest": (34096, 2, (8, 0), (), "FlagRequest", None),
		"FlagStatus": (4240, 2, (3, 0), (), "FlagStatus", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OriginatorDeliveryReportRequested": (35, 2, (11, 0), (), "OriginatorDeliveryReportRequested", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"ReceivedTime": (3590, 2, (7, 0), (), "ReceivedTime", None),
		
		"Recipients": (63508, 2, (9, 0), (), "Recipients", '{0006303B-0000-0000-C000-000000000046}'),
		"ReminderSet": (34051, 2, (11, 0), (), "ReminderSet", None),
		"ReminderTime": (34050, 2, (7, 0), (), "ReminderTime", None),
		
		"ReplyRecipients": (61459, 2, (9, 0), (), "ReplyRecipients", '{0006303B-0000-0000-C000-000000000046}'),
		
		"SaveSentMessageFolder": (62465, 2, (9, 0), (), "SaveSentMessageFolder", '{00063006-0000-0000-C000-000000000046}'),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"SenderName": (3098, 2, (8, 0), (), "SenderName", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		"Sent": (62466, 2, (11, 0), (), "Sent", None),
		"SentOn": (57, 2, (7, 0), (), "SentOn", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"Submitted": (62467, 2, (11, 0), (), "Submitted", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"AutoForwarded": ((5, LCID, 4, 0),()),
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"DeferredDeliveryTime": ((15, LCID, 4, 0),()),
		"DeleteAfterSubmit": ((3585, LCID, 4, 0),()),
		"ExpiryTime": ((21, LCID, 4, 0),()),
		"FlagDueBy": ((48, LCID, 4, 0),()),
		"FlagRequest": ((34096, LCID, 4, 0),()),
		"FlagStatus": ((4240, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"OriginatorDeliveryReportRequested": ((35, LCID, 4, 0),()),
		"ReceivedTime": ((3590, LCID, 4, 0),()),
		"ReminderSet": ((34051, LCID, 4, 0),()),
		"ReminderTime": ((34050, LCID, 4, 0),()),
		"SaveSentMessageFolder": ((62465, LCID, 8, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
	}
class  _NameSpace (DispatchBaseClass) :
	CLSID = IID('{00063002-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{0006308B-0000-0000-C000-000000000046}')
		def AddStore(self, Store=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(8473, LCID, 1, (24, 0), ((12, 1),),Store
			)
 def CreateRecipient(self, RecipientName=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(8458, LCID, 1, (9, 0), ((8, 1),),RecipientName
			)

		if ret is not None:

			ret = Dispatch(ret, 'CreateRecipient', '{00063045-0000-0000-C000-000000000046}')

		return ret
 def GetDefaultFolder(self, FolderType=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(8459, LCID, 1, (9, 0), ((3, 1),),FolderType
			)

		if ret is not None:

			ret = Dispatch(ret, 'GetDefaultFolder', '{00063006-0000-0000-C000-000000000046}')

		return ret
 def GetFolderFromID(self, EntryIDFolder=defaultNamedNotOptArg, EntryIDStore=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(8456, LCID, 1, (9, 0), ((8, 1), (12, 17)),EntryIDFolder
			, EntryIDStore)

		if ret is not None:

			ret = Dispatch(ret, 'GetFolderFromID', '{00063006-0000-0000-C000-000000000046}')

		return ret
 def GetItemFromID(self, EntryIDItem=defaultNamedNotOptArg, EntryIDStore=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(8457, LCID, 1, (9, 0), ((8, 1), (12, 17)),EntryIDItem
			, EntryIDStore)

		if ret is not None:

			ret = Dispatch(ret, 'GetItemFromID', None)

		return ret
 def GetRecipientFromID(self, EntryID=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(8455, LCID, 1, (9, 0), ((8, 1),),EntryID
			)

		if ret is not None:

			ret = Dispatch(ret, 'GetRecipientFromID', '{00063045-0000-0000-C000-000000000046}')

		return ret
 def GetSharedDefaultFolder(self, Recipient=defaultNamedNotOptArg, FolderType=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(8460, LCID, 1, (9, 0), ((9, 1), (3, 1)),Recipient
			, FolderType)

		if ret is not None:

			ret = Dispatch(ret, 'GetSharedDefaultFolder', '{00063006-0000-0000-C000-000000000046}')

		return ret
 def Logoff(self):

		return self._oleobj_.InvokeTypes(8454, LCID, 1, (24, 0), (),)
 def Logon(self, Profile=defaultNamedOptArg, Password=defaultNamedOptArg, ShowDialog=defaultNamedOptArg, NewSession=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(8453, LCID, 1, (24, 0), ((12, 17), (12, 17), (12, 17), (12, 17)),Profile
			, Password, ShowDialog, NewSession)
 def PickFolder(self):

		ret = self._oleobj_.InvokeTypes(8462, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'PickFolder', '{00063006-0000-0000-C000-000000000046}')

		return ret
 def RefreshRemoteHeaders(self):

		return self._oleobj_.InvokeTypes(8471, LCID, 1, (24, 0), (),)
 def RemoveStore(self, Folder=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(8474, LCID, 1, (24, 0), ((9, 1),),Folder
			)

	_prop_map_get_ = {
		
		"AddressLists": (8461, 2, (9, 0), (), "AddressLists", '{00063048-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		
		"CurrentUser": (8449, 2, (9, 0), (), "CurrentUser", '{00063045-0000-0000-C000-000000000046}'),
		
		"Folders": (8451, 2, (9, 0), (), "Folders", '{00063040-0000-0000-C000-000000000046}'),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		
		"SyncObjects": (8472, 2, (9, 0), (), "SyncObjects", '{00063086-0000-0000-C000-000000000046}'),
		"Type": (8452, 2, (8, 0), (), "Type", None),
	}
		_prop_map_put_ = {
	}
class  _NoteItem (DispatchBaseClass) :
	CLSID = IID('{00063025-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061034-0000-0000-C000-000000000046}')
		def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Color": (35584, 2, (3, 0), (), "Color", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"Height": (35587, 2, (3, 0), (), "Height", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		"Left": (35588, 2, (3, 0), (), "Left", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Subject": (63392, 2, (8, 0), (), "Subject", None),
		"Top": (35589, 2, (3, 0), (), "Top", None),
		"Width": (35586, 2, (3, 0), (), "Width", None),
	}
		_prop_map_put_ = {
		"Body": ((37120, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Color": ((35584, LCID, 4, 0),()),
		"Height": ((35587, LCID, 4, 0),()),
		"Left": ((35588, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Top": ((35589, LCID, 4, 0),()),
		"Width": ((35586, LCID, 4, 0),()),
	}
class  _OutlookBarGroups (DispatchBaseClass) :
	CLSID = IID('{00063072-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063056-0000-0000-C000-000000000046}')
		
	def Add(self, Name=defaultNamedNotOptArg, Index=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(95, LCID, 1, (9, 0), ((8, 1), (12, 17)),Name
			, Index)

		if ret is not None:

			ret = Dispatch(ret, 'Add', '{00063073-0000-0000-C000-000000000046}')

		return ret
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', '{00063073-0000-0000-C000-000000000046}')

		return ret
 def Remove(self, Index=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(84, LCID, 1, (24, 0), ((12, 1),),Index
			)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  _OutlookBarPane (DispatchBaseClass) :
	CLSID = IID('{00063070-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063055-0000-0000-C000-000000000046}')
		_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		
		"Contents": (8448, 2, (9, 0), (), "Contents", '{00063071-0000-0000-C000-000000000046}'),
		
		"CurrentGroup": (8449, 2, (9, 0), (), "CurrentGroup", '{00063073-0000-0000-C000-000000000046}'),
		"Name": (0, 2, (8, 0), (), "Name", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Visible": (8451, 2, (11, 0), (), "Visible", None),
	}
		_prop_map_put_ = {
		"CurrentGroup": ((8449, LCID, 8, 0),()),
		"Visible": ((8451, LCID, 4, 0),()),
	}
		
	def __call__(self):

		return self._ApplyTypes_(*(0, 2, (8, 0), (), "Name", None))
 def __unicode__(self, *args):

		try:

			return str(self.__call__(*args))

		except pythoncom.com_error:

			return repr(self)
 def __str__(self, *args):

		return str(self.__unicode__(*args))
 def __int__(self, *args):

		return int(self.__call__(*args))

class  _OutlookBarShortcuts (DispatchBaseClass) :
	CLSID = IID('{00063074-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063057-0000-0000-C000-000000000046}')
		
	def Add(self, Target=defaultNamedNotOptArg, Name=defaultNamedNotOptArg, Index=defaultNamedOptArg):

		ret = self._oleobj_.InvokeTypes(95, LCID, 1, (9, 0), ((12, 1), (8, 1), (12, 17)),Target
			, Name, Index)

		if ret is not None:

			ret = Dispatch(ret, 'Add', '{00063075-0000-0000-C000-000000000046}')

		return ret
 def Item(self, Index=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(81, LCID, 1, (9, 0), ((12, 1),),Index
			)

		if ret is not None:

			ret = Dispatch(ret, 'Item', '{00063075-0000-0000-C000-000000000046}')

		return ret
 def Remove(self, Index=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(84, LCID, 1, (24, 0), ((12, 1),),Index
			)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Count": (80, 2, (3, 0), (), "Count", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
		
	def __getitem__(self, item):

		return self._get_good_object_(self._oleobj_.Invoke(*(81, LCID, 1, 1, item)), "Item")
 def __len__(self):

		return self._ApplyTypes_(*(80, 2, (3, 0), (), "Count", None))
 def __bool__(self):

		return True

 
class  _PostItem (DispatchBaseClass) :
	CLSID = IID('{00063024-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{0006103A-0000-0000-C000-000000000046}')
		def ClearConversationIndex(self):

		return self._oleobj_.InvokeTypes(63522, LCID, 1, (24, 0), (),)
 def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def Forward(self):

		ret = self._oleobj_.InvokeTypes(63507, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Forward', '{00061033-0000-0000-C000-000000000046}')

		return ret
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def Post(self):

		return self._oleobj_.InvokeTypes(61557, LCID, 1, (24, 0), (),)
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Reply(self):

		ret = self._oleobj_.InvokeTypes(63504, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Reply', '{00061033-0000-0000-C000-000000000046}')

		return ret
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		"ExpiryTime": (21, 2, (7, 0), (), "ExpiryTime", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"HTMLBody": (62468, 2, (8, 0), (), "HTMLBody", None),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"ReceivedTime": (3590, 2, (7, 0), (), "ReceivedTime", None),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"SenderName": (3098, 2, (8, 0), (), "SenderName", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		"SentOn": (57, 2, (7, 0), (), "SentOn", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"ExpiryTime": ((21, LCID, 4, 0),()),
		"HTMLBody": ((62468, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
	}
class  _RemoteItem (DispatchBaseClass) :
	CLSID = IID('{00063023-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061060-0000-0000-C000-000000000046}')
		def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"HasAttachment": (36615, 2, (11, 0), (), "HasAttachment", None),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"RemoteMessageClass": (36610, 2, (8, 0), (), "RemoteMessageClass", None),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"TransferSize": (36613, 2, (3, 0), (), "TransferSize", None),
		"TransferTime": (36612, 2, (3, 0), (), "TransferTime", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
	}
class  _ReportItem (DispatchBaseClass) :
	CLSID = IID('{00063026-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061035-0000-0000-C000-000000000046}')
		def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
	}
class  _SyncObject (DispatchBaseClass) :
	CLSID = IID('{00063083-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00063084-0000-0000-C000-000000000046}')
		def Start(self):

		return self._oleobj_.InvokeTypes(8449, LCID, 1, (24, 0), (),)
 def Stop(self):

		return self._oleobj_.InvokeTypes(8450, LCID, 1, (24, 0), (),)

	_prop_map_get_ = {
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Name": (8448, 2, (8, 0), (), "Name", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
	}
class  _TaskItem (DispatchBaseClass) :
	CLSID = IID('{00063035-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061032-0000-0000-C000-000000000046}')
		
	def Assign(self):

		ret = self._oleobj_.InvokeTypes(63008, LCID, 1, (13, 0), (),)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Assign', '{00061032-0000-0000-C000-000000000046}')

		return ret
 def CancelResponseState(self):

		return self._oleobj_.InvokeTypes(63010, LCID, 1, (24, 0), (),)
 def ClearRecurrencePattern(self):

		return self._oleobj_.InvokeTypes(61605, LCID, 1, (24, 0), (),)
 def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def GetRecurrencePattern(self):

		ret = self._oleobj_.InvokeTypes(61604, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'GetRecurrencePattern', '{00063044-0000-0000-C000-000000000046}')

		return ret
 def MarkComplete(self):

		return self._oleobj_.InvokeTypes(62989, LCID, 1, (24, 0), (),)
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Respond(self, Response=defaultNamedNotOptArg, fNoUI=defaultNamedNotOptArg, fAdditionalTextDialog=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(63009, LCID, 1, (13, 0), ((3, 1), (12, 1), (12, 1)),Response
			, fNoUI, fAdditionalTextDialog)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'Respond', '{00061032-0000-0000-C000-000000000046}')

		return ret
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)
 def Send(self):

		return self._oleobj_.InvokeTypes(61557, LCID, 1, (24, 0), (),)
 def SkipRecurrence(self):

		return self._oleobj_.InvokeTypes(63012, LCID, 1, (11, 0), (),)
 def StatusReport(self):

		ret = self._oleobj_.InvokeTypes(62994, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'StatusReport', None)

		return ret

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		"ActualWork": (33040, 2, (3, 0), (), "ActualWork", None),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"CardData": (33067, 2, (8, 0), (), "CardData", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"Complete": (33052, 2, (11, 0), (), "Complete", None),
		"ContactNames": (34108, 2, (8, 0), (), "ContactNames", None),
		"Contacts": (34106, 2, (8, 0), (), "Contacts", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"DateCompleted": (33039, 2, (7, 0), (), "DateCompleted", None),
		"DelegationState": (33066, 2, (3, 0), (), "DelegationState", None),
		"Delegator": (33057, 2, (8, 0), (), "Delegator", None),
		"DueDate": (33029, 2, (7, 0), (), "DueDate", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"IsRecurring": (62999, 2, (11, 0), (), "IsRecurring", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"Ordinal": (33059, 2, (3, 0), (), "Ordinal", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Owner": (33055, 2, (8, 0), (), "Owner", None),
		"Ownership": (33065, 2, (3, 0), (), "Ownership", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"PercentComplete": (63007, 2, (3, 0), (), "PercentComplete", None),
		
		"Recipients": (63508, 2, (9, 0), (), "Recipients", '{0006303B-0000-0000-C000-000000000046}'),
		"ReminderOverrideDefault": (34076, 2, (11, 0), (), "ReminderOverrideDefault", None),
		"ReminderPlaySound": (34078, 2, (11, 0), (), "ReminderPlaySound", None),
		"ReminderSet": (34051, 2, (11, 0), (), "ReminderSet", None),
		"ReminderSoundFile": (34079, 2, (8, 0), (), "ReminderSoundFile", None),
		"ReminderTime": (34050, 2, (7, 0), (), "ReminderTime", None),
		"ResponseState": (63011, 2, (3, 0), (), "ResponseState", None),
		"Role": (33063, 2, (8, 0), (), "Role", None),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"SchedulePlusPriority": (33071, 2, (8, 0), (), "SchedulePlusPriority", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"StartDate": (33028, 2, (7, 0), (), "StartDate", None),
		"Status": (33025, 2, (3, 0), (), "Status", None),
		"StatusOnCompletionRecipients": (3586, 2, (8, 0), (), "StatusOnCompletionRecipients", None),
		"StatusUpdateRecipients": (3587, 2, (8, 0), (), "StatusUpdateRecipients", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"TeamTask": (33027, 2, (11, 0), (), "TeamTask", None),
		"TotalWork": (33041, 2, (3, 0), (), "TotalWork", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"ActualWork": ((33040, LCID, 4, 0),()),
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"CardData": ((33067, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"Complete": ((33052, LCID, 4, 0),()),
		"ContactNames": ((34108, LCID, 4, 0),()),
		"Contacts": ((34106, LCID, 4, 0),()),
		"DateCompleted": ((33039, LCID, 4, 0),()),
		"DueDate": ((33029, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"Ordinal": ((33059, LCID, 4, 0),()),
		"Owner": ((33055, LCID, 4, 0),()),
		"PercentComplete": ((63007, LCID, 4, 0),()),
		"ReminderOverrideDefault": ((34076, LCID, 4, 0),()),
		"ReminderPlaySound": ((34078, LCID, 4, 0),()),
		"ReminderSet": ((34051, LCID, 4, 0),()),
		"ReminderSoundFile": ((34079, LCID, 4, 0),()),
		"ReminderTime": ((34050, LCID, 4, 0),()),
		"Role": ((33063, LCID, 4, 0),()),
		"SchedulePlusPriority": ((33071, LCID, 4, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"StartDate": ((33028, LCID, 4, 0),()),
		"Status": ((33025, LCID, 4, 0),()),
		"StatusOnCompletionRecipients": ((3586, LCID, 4, 0),()),
		"StatusUpdateRecipients": ((3587, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"TeamTask": ((33027, LCID, 4, 0),()),
		"TotalWork": ((33041, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
	}
class  _TaskRequestAcceptItem (DispatchBaseClass) :
	CLSID = IID('{00063038-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061052-0000-0000-C000-000000000046}')
		def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def GetAssociatedTask(self, AddToTaskList=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61460, LCID, 1, (13, 0), ((11, 1),),AddToTaskList
			)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'GetAssociatedTask', '{00061032-0000-0000-C000-000000000046}')

		return ret
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
	}
class  _TaskRequestDeclineItem (DispatchBaseClass) :
	CLSID = IID('{00063039-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061053-0000-0000-C000-000000000046}')
		def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def GetAssociatedTask(self, AddToTaskList=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61460, LCID, 1, (13, 0), ((11, 1),),AddToTaskList
			)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'GetAssociatedTask', '{00061032-0000-0000-C000-000000000046}')

		return ret
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
	}
class  _TaskRequestItem (DispatchBaseClass) :
	CLSID = IID('{00063036-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061050-0000-0000-C000-000000000046}')
		def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def GetAssociatedTask(self, AddToTaskList=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61460, LCID, 1, (13, 0), ((11, 1),),AddToTaskList
			)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'GetAssociatedTask', '{00061032-0000-0000-C000-000000000046}')

		return ret
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
	}
class  _TaskRequestUpdateItem (DispatchBaseClass) :
	CLSID = IID('{00063037-0000-0000-C000-000000000046}')
		coclass_clsid = IID('{00061051-0000-0000-C000-000000000046}')
		def Close(self, SaveMode=defaultNamedNotOptArg):

		return self._oleobj_.InvokeTypes(61475, LCID, 1, (24, 0), ((3, 1),),SaveMode
			)
 def Copy(self):

		ret = self._oleobj_.InvokeTypes(61490, LCID, 1, (9, 0), (),)

		if ret is not None:

			ret = Dispatch(ret, 'Copy', None)

		return ret
 def Delete(self):

		return self._oleobj_.InvokeTypes(61514, LCID, 1, (24, 0), (),)
 def Display(self, Modal=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61606, LCID, 1, (24, 0), ((12, 17),),Modal
			)
 def GetAssociatedTask(self, AddToTaskList=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61460, LCID, 1, (13, 0), ((11, 1),),AddToTaskList
			)

		if ret is not None:

			
			try:

				ret = ret.QueryInterface(pythoncom.IID_IDispatch)

			except pythoncom.error:

				return ret

			ret = Dispatch(ret, 'GetAssociatedTask', '{00061032-0000-0000-C000-000000000046}')

		return ret
 def Move(self, DestFldr=defaultNamedNotOptArg):

		ret = self._oleobj_.InvokeTypes(61492, LCID, 1, (9, 0), ((9, 1),),DestFldr
			)

		if ret is not None:

			ret = Dispatch(ret, 'Move', None)

		return ret
 def PrintOut(self):

		return self._oleobj_.InvokeTypes(61491, LCID, 1, (24, 0), (),)
 def Save(self):

		return self._oleobj_.InvokeTypes(61512, LCID, 1, (24, 0), (),)
 def SaveAs(self, Path=defaultNamedNotOptArg, Type=defaultNamedOptArg):

		return self._oleobj_.InvokeTypes(61521, LCID, 1, (24, 0), ((8, 1), (12, 17)),Path
			, Type)

	_prop_map_get_ = {
		
		"Actions": (63511, 2, (9, 0), (), "Actions", '{0006303E-0000-0000-C000-000000000046}'),
		
		"Application": (61440, 2, (9, 0), (), "Application", '{00063001-0000-0000-C000-000000000046}'),
		
		"Attachments": (63509, 2, (9, 0), (), "Attachments", '{0006303C-0000-0000-C000-000000000046}'),
		"BillingInformation": (34101, 2, (8, 0), (), "BillingInformation", None),
		"Body": (37120, 2, (8, 0), (), "Body", None),
		"Categories": (36865, 2, (8, 0), (), "Categories", None),
		"Class": (61450, 2, (3, 0), (), "Class", None),
		"Companies": (34107, 2, (8, 0), (), "Companies", None),
		"ConversationIndex": (113, 2, (8, 0), (), "ConversationIndex", None),
		"ConversationTopic": (112, 2, (8, 0), (), "ConversationTopic", None),
		"CreationTime": (12295, 2, (7, 0), (), "CreationTime", None),
		"EntryID": (61470, 2, (8, 0), (), "EntryID", None),
		
		"FormDescription": (61589, 2, (9, 0), (), "FormDescription", '{00063046-0000-0000-C000-000000000046}'),
		
		"GetInspector": (61502, 2, (9, 0), (), "GetInspector", '{00063005-0000-0000-C000-000000000046}'),
		"Importance": (23, 2, (3, 0), (), "Importance", None),
		"LastModificationTime": (12296, 2, (7, 0), (), "LastModificationTime", None),
		
		"Links": (62469, 2, (9, 0), (), "Links", '{0006308A-0000-0000-C000-000000000046}'),
		"MAPIOBJECT": (61696, 2, (13, 0), (), "MAPIOBJECT", None),
		"MessageClass": (26, 2, (8, 0), (), "MessageClass", None),
		"Mileage": (34100, 2, (8, 0), (), "Mileage", None),
		"NoAging": (34062, 2, (11, 0), (), "NoAging", None),
		"OutlookInternalVersion": (34130, 2, (3, 0), (), "OutlookInternalVersion", None),
		"OutlookVersion": (34132, 2, (8, 0), (), "OutlookVersion", None),
		"Parent": (61441, 2, (9, 0), (), "Parent", None),
		"Saved": (61603, 2, (11, 0), (), "Saved", None),
		"Sensitivity": (54, 2, (3, 0), (), "Sensitivity", None),
		
		"Session": (61451, 2, (9, 0), (), "Session", '{00063002-0000-0000-C000-000000000046}'),
		"Size": (3592, 2, (3, 0), (), "Size", None),
		"Subject": (55, 2, (8, 0), (), "Subject", None),
		"UnRead": (61468, 2, (11, 0), (), "UnRead", None),
		
		"UserProperties": (63510, 2, (9, 0), (), "UserProperties", '{0006303D-0000-0000-C000-000000000046}'),
	}
		_prop_map_put_ = {
		"BillingInformation": ((34101, LCID, 4, 0),()),
		"Body": ((37120, LCID, 4, 0),()),
		"Categories": ((36865, LCID, 4, 0),()),
		"Companies": ((34107, LCID, 4, 0),()),
		"Importance": ((23, LCID, 4, 0),()),
		"MessageClass": ((26, LCID, 4, 0),()),
		"Mileage": ((34100, LCID, 4, 0),()),
		"NoAging": ((34062, LCID, 4, 0),()),
		"Sensitivity": ((54, LCID, 4, 0),()),
		"Subject": ((55, LCID, 4, 0),()),
		"UnRead": ((61468, LCID, 4, 0),()),
	}
from win32com.client import CoClassBaseClass class  Application (CoClassBaseClass) :
	CLSID = IID('{0006F03A-0000-0000-C000-000000000046}')
		coclass_sources = [
		ApplicationEvents,
	]
		default_source = ApplicationEvents
		coclass_interfaces = [
		_Application,
	]
		default_interface = _Application
class  AppointmentItem (CoClassBaseClass) :
	CLSID = IID('{00061030-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_AppointmentItem,
	]
		default_interface = _AppointmentItem
class  ContactItem (CoClassBaseClass) :
	CLSID = IID('{00061031-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_ContactItem,
	]
		default_interface = _ContactItem
class  DistListItem (CoClassBaseClass) :
	CLSID = IID('{0006103C-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_DistListItem,
	]
		default_interface = _DistListItem
class  DocumentItem (CoClassBaseClass) :
	CLSID = IID('{00061061-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_DocumentItem,
	]
		default_interface = _DocumentItem
class  Explorer (CoClassBaseClass) :
	CLSID = IID('{00063050-0000-0000-C000-000000000046}')
		coclass_sources = [
		ExplorerEvents,
	]
		default_source = ExplorerEvents
		coclass_interfaces = [
		_Explorer,
	]
		default_interface = _Explorer
class  Explorers (CoClassBaseClass) :
	CLSID = IID('{00063053-0000-0000-C000-000000000046}')
		coclass_sources = [
		ExplorersEvents,
	]
		default_source = ExplorersEvents
		coclass_interfaces = [
		_Explorers,
	]
		default_interface = _Explorers
class  Folders (CoClassBaseClass) :
	CLSID = IID('{00063051-0000-0000-C000-000000000046}')
		coclass_sources = [
		FoldersEvents,
	]
		default_source = FoldersEvents
		coclass_interfaces = [
		_Folders,
	]
		default_interface = _Folders
class  Inspector (CoClassBaseClass) :
	CLSID = IID('{00063058-0000-0000-C000-000000000046}')
		coclass_sources = [
		InspectorEvents,
	]
		default_source = InspectorEvents
		coclass_interfaces = [
		_Inspector,
	]
		default_interface = _Inspector
class  Inspectors (CoClassBaseClass) :
	CLSID = IID('{00063054-0000-0000-C000-000000000046}')
		coclass_sources = [
		InspectorsEvents,
	]
		default_source = InspectorsEvents
		coclass_interfaces = [
		_Inspectors,
	]
		default_interface = _Inspectors
class  Items (CoClassBaseClass) :
	CLSID = IID('{00063052-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemsEvents,
	]
		default_source = ItemsEvents
		coclass_interfaces = [
		_Items,
	]
		default_interface = _Items
class  JournalItem (CoClassBaseClass) :
	CLSID = IID('{00061037-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_JournalItem,
	]
		default_interface = _JournalItem
class  MailItem (CoClassBaseClass) :
	CLSID = IID('{00061033-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_MailItem,
	]
		default_interface = _MailItem
class  MeetingItem (CoClassBaseClass) :
	CLSID = IID('{00061036-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_MeetingItem,
	]
		default_interface = _MeetingItem
class  NameSpace (CoClassBaseClass) :
	CLSID = IID('{0006308B-0000-0000-C000-000000000046}')
		coclass_sources = [
		NameSpaceEvents,
	]
		default_source = NameSpaceEvents
		coclass_interfaces = [
		_NameSpace,
	]
		default_interface = _NameSpace
class  NoteItem (CoClassBaseClass) :
	CLSID = IID('{00061034-0000-0000-C000-000000000046}')
		coclass_sources = [
	]
		coclass_interfaces = [
		_NoteItem,
	]
		default_interface = _NoteItem
class  OutlookBarGroups (CoClassBaseClass) :
	CLSID = IID('{00063056-0000-0000-C000-000000000046}')
		coclass_sources = [
		OutlookBarGroupsEvents,
	]
		default_source = OutlookBarGroupsEvents
		coclass_interfaces = [
		_OutlookBarGroups,
	]
		default_interface = _OutlookBarGroups
class  OutlookBarPane (CoClassBaseClass) :
	CLSID = IID('{00063055-0000-0000-C000-000000000046}')
		coclass_sources = [
		OutlookBarPaneEvents,
	]
		default_source = OutlookBarPaneEvents
		coclass_interfaces = [
		_OutlookBarPane,
	]
		default_interface = _OutlookBarPane
class  OutlookBarShortcuts (CoClassBaseClass) :
	CLSID = IID('{00063057-0000-0000-C000-000000000046}')
		coclass_sources = [
		OutlookBarShortcutsEvents,
	]
		default_source = OutlookBarShortcutsEvents
		coclass_interfaces = [
		_OutlookBarShortcuts,
	]
		default_interface = _OutlookBarShortcuts
class  PostItem (CoClassBaseClass) :
	CLSID = IID('{0006103A-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_PostItem,
	]
		default_interface = _PostItem
class  RemoteItem (CoClassBaseClass) :
	CLSID = IID('{00061060-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_RemoteItem,
	]
		default_interface = _RemoteItem
class  ReportItem (CoClassBaseClass) :
	CLSID = IID('{00061035-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_ReportItem,
	]
		default_interface = _ReportItem
class  SyncObject (CoClassBaseClass) :
	CLSID = IID('{00063084-0000-0000-C000-000000000046}')
		coclass_sources = [
		SyncObjectEvents,
	]
		default_source = SyncObjectEvents
		coclass_interfaces = [
		_SyncObject,
	]
		default_interface = _SyncObject
class  TaskItem (CoClassBaseClass) :
	CLSID = IID('{00061032-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_TaskItem,
	]
		default_interface = _TaskItem
class  TaskRequestAcceptItem (CoClassBaseClass) :
	CLSID = IID('{00061052-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_TaskRequestAcceptItem,
	]
		default_interface = _TaskRequestAcceptItem
class  TaskRequestDeclineItem (CoClassBaseClass) :
	CLSID = IID('{00061053-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_TaskRequestDeclineItem,
	]
		default_interface = _TaskRequestDeclineItem
class  TaskRequestItem (CoClassBaseClass) :
	CLSID = IID('{00061050-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_TaskRequestItem,
	]
		default_interface = _TaskRequestItem
class  TaskRequestUpdateItem (CoClassBaseClass) :
	CLSID = IID('{00061051-0000-0000-C000-000000000046}')
		coclass_sources = [
		ItemEvents,
	]
		default_source = ItemEvents
		coclass_interfaces = [
		_TaskRequestUpdateItem,
	]
		default_interface = _TaskRequestUpdateItem
class  _DocSiteControl (CoClassBaseClass) :
	CLSID = IID('{0006F024-0000-0000-C000-000000000046}')
		coclass_sources = [
		_DDocSiteControlEvents,
	]
		default_source = _DDocSiteControlEvents
		coclass_interfaces = [
		_DDocSiteControl,
	]
		default_interface = _DDocSiteControl
class  _RecipientControl (CoClassBaseClass) :
	CLSID = IID('{0006F023-0000-0000-C000-000000000046}')
		coclass_sources = [
		_DRecipientControlEvents,
	]
		default_source = _DRecipientControlEvents
		coclass_interfaces = [
		_DRecipientControl,
	]
		default_interface = _DRecipientControl
Action_vtables_dispatch_ = 1 Action_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'CopyLike' , 'CopyLike' , ), 100, (100, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'CopyLike' , 'CopyLike' , ), 100, (100, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Enabled' , 'Enabled' , ), 103, (103, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Enabled' , 'Enabled' , ), 103, (103, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 12289, (12289, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 12289, (12289, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Prefix' , 'Prefix' , ), 61, (61, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Prefix' , 'Prefix' , ), 61, (61, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ReplyStyle' , 'ReplyStyle' , ), 101, (101, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ReplyStyle' , 'ReplyStyle' , ), 101, (101, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'ResponseStyle' , 'ResponseStyle' , ), 102, (102, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'ResponseStyle' , 'ResponseStyle' , ), 102, (102, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'ShowOn' , 'ShowOn' , ), 105, (105, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'ShowOn' , 'ShowOn' , ), 105, (105, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 108, (108, (), [ ], 1 , 1 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Execute' , 'Item' , ), 106, (106, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
] Actions_vtables_dispatch_ = 1 Actions_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, "IID('{00063043-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Add' , 'Action' , ), 100, (100, (), [ (16393, 10, None, "IID('{00063043-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Remove' , 'Index' , ), 82, (82, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
] AddressEntries_vtables_dispatch_ = 1 AddressEntries_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, "IID('{0006304B-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'RawTable' , 'RawTable' , ), 90, (90, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 64 , )),
	(( 'Add' , 'Type' , 'Name' , 'Address' , 'Entry' , 
			), 95, (95, (), [ (8, 1, None, None) , (12, 17, None, None) , (12, 17, None, None) , (16393, 10, None, "IID('{0006304B-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 2 , 56 , (3, 0, None, None) , 0 , )),
	(( 'GetFirst' , 'AddressEntry' , ), 86, (86, (), [ (16393, 10, None, "IID('{0006304B-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'GetLast' , 'AddressEntry' , ), 88, (88, (), [ (16393, 10, None, "IID('{0006304B-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'GetNext' , 'AddressEntry' , ), 87, (87, (), [ (16393, 10, None, "IID('{0006304B-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'GetPrevious' , 'AddressEntry' , ), 89, (89, (), [ (16393, 10, None, "IID('{0006304B-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Sort' , 'Property' , 'Order' , ), 97, (97, (), [ (12, 17, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 2 , 76 , (3, 0, None, None) , 0 , )),
] AddressEntry_vtables_dispatch_ = 1 AddressEntry_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Address' , 'Address' , ), 12291, (12291, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Address' , 'Address' , ), 12291, (12291, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'DisplayType' , 'DisplayType' , ), 14592, (14592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'ID' , 'ID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Manager' , 'Manager' , ), 771, (771, (), [ (16393, 10, None, "IID('{0006304B-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 64 , (3, 0, None, None) , 64 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (13, 1, None, None) , ], 1 , 4 , 4 , 0 , 68 , (3, 0, None, None) , 64 , )),
	(( 'Members' , 'Members' , ), 772, (772, (), [ (16393, 10, None, "IID('{0006304A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 12289, (12289, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 12289, (12289, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'Type' , 'Type' , ), 12290, (12290, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'Type' , 'Type' , ), 12290, (12290, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 770, (770, (), [ ], 1 , 1 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'Details' , 'HWnd' , ), 769, (769, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 96 , (3, 0, None, None) , 0 , )),
	(( 'GetFreeBusy' , 'Start' , 'MinPerChar' , 'CompleteFormat' , 'FreeBusyInfo' , 
			), 774, (774, (), [ (7, 1, None, None) , (3, 1, None, None) , (12, 17, None, None) , (16392, 10, None, None) , ], 1 , 1 , 4 , 1 , 100 , (3, 0, None, None) , 0 , )),
	(( 'Update' , 'MakePermanent' , 'Refresh' , ), 768, (768, (), [ (12, 17, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 2 , 104 , (3, 0, None, None) , 0 , )),
	(( 'UpdateFreeBusy' , ), 775, (775, (), [ ], 1 , 1 , 4 , 0 , 108 , (3, 0, None, None) , 64 , )),
] AddressList_vtables_dispatch_ = 1 AddressList_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'AddressEntries' , 'AddressEntries' , ), 256, (256, (), [ (16393, 10, None, "IID('{0006304A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'ID' , 'ID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Index' , 'Index' , ), 91, (91, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'IsReadOnly' , 'IsReadOnly' , ), 61463, (61463, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 12289, (12289, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
] AddressLists_vtables_dispatch_ = 1 AddressLists_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, "IID('{00063049-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
] Attachment_vtables_dispatch_ = 1 Attachment_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 113, (113, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'DisplayName' , 'DisplayName' , ), 12289, (12289, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'DisplayName' , 'DisplayName' , ), 12289, (12289, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'FileName' , 'FileName' , ), 14084, (14084, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Index' , 'Index' , ), 91, (91, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 64 , )),
	(( 'PathName' , 'PathName' , ), 14088, (14088, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Position' , 'Position' , ), 114, (114, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Position' , 'Position' , ), 114, (114, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Type' , 'Type' , ), 14085, (14085, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 105, (105, (), [ ], 1 , 1 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'SaveAsFile' , 'Path' , ), 104, (104, (), [ (8, 1, None, None) , ], 1 , 1 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
] Attachments_vtables_dispatch_ = 1 Attachments_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, "IID('{00063007-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Add' , 'Source' , 'Type' , 'Position' , 'DisplayName' , 
			'Attachment' , ), 101, (101, (), [ (12, 1, None, None) , (12, 17, None, None) , (12, 17, None, None) , 
			(12, 17, None, None) , (16393, 10, None, "IID('{00063007-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 3 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Remove' , 'Index' , ), 84, (84, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
] Exception_vtables_dispatch_ = 1 Exception_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'AppointmentItem' , 'AppointmentItem' , ), 8193, (8193, (), [ (16397, 10, None, "IID('{00061030-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Deleted' , 'Deleted' , ), 8194, (8194, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'OriginalDate' , 'OriginalDate' , ), 8192, (8192, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
] Exceptions_vtables_dispatch_ = 1 Exceptions_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, "IID('{0006304D-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
] FormDescription_vtables_dispatch_ = 1 FormDescription_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Category' , 'Category' , ), 13060, (13060, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Category' , 'Category' , ), 13060, (13060, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'CategorySub' , 'CategorySub' , ), 13061, (13061, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'CategorySub' , 'CategorySub' , ), 13061, (13061, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Comment' , 'Comment' , ), 12292, (12292, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Comment' , 'Comment' , ), 12292, (12292, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'ContactName' , 'ContactName' , ), 13059, (13059, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'ContactName' , 'ContactName' , ), 13059, (13059, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'DisplayName' , 'DisplayName' , ), 12289, (12289, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'DisplayName' , 'DisplayName' , ), 12289, (12289, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'Hidden' , 'Hidden' , ), 13063, (13063, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'Hidden' , 'Hidden' , ), 13063, (13063, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'Icon' , 'Icon' , ), 4093, (4093, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'Icon' , 'Icon' , ), 4093, (4093, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'Locked' , 'Locked' , ), 102, (102, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'Locked' , 'Locked' , ), 102, (102, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'MiniIcon' , 'MiniIcon' , ), 4092, (4092, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'MiniIcon' , 'MiniIcon' , ), 4092, (4092, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 61469, (61469, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 61469, (61469, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'Number' , 'Number' , ), 104, (104, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Number' , 'Number' , ), 104, (104, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'OneOff' , 'OneOff' , ), 101, (101, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'OneOff' , 'OneOff' , ), 101, (101, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'Password' , 'Password' , ), 103, (103, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'Password' , 'Password' , ), 103, (103, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'ScriptText' , 'ScriptText' , ), 109, (109, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Template' , 'Template' , ), 106, (106, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Template' , 'Template' , ), 106, (106, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'UseWordMail' , 'UseWordMail' , ), 105, (105, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'UseWordMail' , 'UseWordMail' , ), 105, (105, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Version' , 'Version' , ), 13057, (13057, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Version' , 'Version' , ), 13057, (13057, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'PublishForm' , 'Registry' , 'Folder' , ), 107, (107, (), [ (3, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 180 , (3, 0, None, None) , 0 , )),
] Link_vtables_dispatch_ = 1 Link_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 109, (109, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 12289, (12289, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Type' , 'Type' , ), 8449, (8449, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Item' , ), 8450, (8450, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
] Links_vtables_dispatch_ = 1 Links_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, "IID('{00063089-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Add' , 'Item' , 'Link' , ), 95, (95, (), [ (9, 1, None, None) , 
			(16393, 10, None, "IID('{00063089-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Remove' , 'Index' , ), 84, (84, (), [ (12, 1, None, None) , ], 1 , 1 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
] MAPIFolder_vtables_dispatch_ = 1 MAPIFolder_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'DefaultItemType' , 'DefaultItemType' , ), 12550, (12550, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'DefaultMessageClass' , 'DefaultMessageClass' , ), 12551, (12551, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Description' , 'Description' , ), 12292, (12292, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Description' , 'Description' , ), 12292, (12292, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Folders' , 'Folders' , ), 8451, (8451, (), [ (16393, 10, None, "IID('{00063040-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Items' , 'Items' , ), 12544, (12544, (), [ (16393, 10, None, "IID('{00063041-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 12289, (12289, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 12289, (12289, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'StoreID' , 'StoreID' , ), 12552, (12552, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'UnReadItemCount' , 'UnReadItemCount' , ), 13827, (13827, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'CopyTo' , 'DestinationFolder' , 'Folder' , ), 61490, (61490, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61509, (61509, (), [ ], 1 , 1 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'Display' , ), 12548, (12548, (), [ ], 1 , 1 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'GetExplorer' , 'DisplayMode' , 'Explorer' , ), 12545, (12545, (), [ (12, 17, None, None) , 
			(16393, 10, None, "IID('{00063003-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 1 , 100 , (3, 0, None, None) , 0 , )),
	(( 'MoveTo' , 'DestinationFolder' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'UserPermissions' , 'UserPermissions' , ), 12561, (12561, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 64 , )),
	(( 'WebViewOn' , 'WebViewOn' , ), 12562, (12562, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'WebViewOn' , 'WebViewOn' , ), 12562, (12562, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'WebViewURL' , 'WebViewURL' , ), 12563, (12563, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 0 , )),
	(( 'WebViewURL' , 'WebViewURL' , ), 12563, (12563, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'WebViewAllowNavigation' , 'WebViewAllowNavigation' , ), 12564, (12564, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'WebViewAllowNavigation' , 'WebViewAllowNavigation' , ), 12564, (12564, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'AddToPFFavorites' , ), 12565, (12565, (), [ ], 1 , 1 , 4 , 0 , 136 , (3, 0, None, None) , 64 , )),
] OutlookBarGroup_vtables_dispatch_ = 1 OutlookBarGroup_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 0, (0, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 0, (0, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Shortcuts' , 'Shortcuts' , ), 8450, (8450, (), [ (16393, 10, None, "IID('{00063074-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'ViewType' , 'ViewType' , ), 8451, (8451, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'ViewType' , 'ViewType' , ), 8451, (8451, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
] OutlookBarShortcut_vtables_dispatch_ = 1 OutlookBarShortcut_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 0, (0, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 0, (0, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Target' , 'Target' , ), 8448, (8448, (), [ (16396, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
] OutlookBarStorage_vtables_dispatch_ = 1 OutlookBarStorage_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Groups' , 'Groups' , ), 0, (0, (), [ (16393, 10, None, "IID('{00063072-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
] Pages_vtables_dispatch_ = 1 Pages_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Add' , 'Name' , 'Page' , ), 300, (300, (), [ (12, 17, None, None) , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 1 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Remove' , 'Index' , ), 301, (301, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
] Panes_vtables_dispatch_ = 1 Panes_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
] PropertyPageSite_vtables_dispatch_ = 1 PropertyPageSite_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'OnStatusChange' , ), 8448, (8448, (), [ ], 1 , 1 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
] PropertyPages_vtables_dispatch_ = 1 PropertyPages_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Add' , 'Page' , 'Title' , ), 95, (95, (), [ (12, 1, None, None) , 
			(8, 17, None, None) , ], 1 , 1 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Remove' , 'Index' , ), 84, (84, (), [ (12, 1, None, None) , ], 1 , 1 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
] Recipient_vtables_dispatch_ = 1 Recipient_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 109, (109, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Address' , 'Address' , ), 12291, (12291, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'AddressEntry' , 'AddressEntry' , ), 121, (121, (), [ (16393, 10, None, "IID('{0006304B-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'AddressEntry' , 'AddressEntry' , ), 121, (121, (), [ (9, 1, None, "IID('{0006304B-0000-0000-C000-000000000046}')") , ], 1 , 8 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'AutoResponse' , 'AutoResponse' , ), 106, (106, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'AutoResponse' , 'AutoResponse' , ), 106, (106, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'DisplayType' , 'DisplayType' , ), 14592, (14592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Index' , 'Index' , ), 91, (91, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'MeetingResponseStatus' , 'MeetingResponseStatus' , ), 102, (102, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 12289, (12289, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'Resolved' , 'Resolved' , ), 100, (100, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'TrackingStatus' , 'TrackingStatus' , ), 118, (118, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'TrackingStatus' , 'TrackingStatus' , ), 118, (118, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'TrackingStatusTime' , 'TrackingStatusTime' , ), 119, (119, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'TrackingStatusTime' , 'TrackingStatusTime' , ), 119, (119, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'Type' , 'Type' , ), 3093, (3093, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Type' , 'Type' , ), 3093, (3093, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 110, (110, (), [ ], 1 , 1 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'FreeBusy' , 'Start' , 'MinPerChar' , 'CompleteFormat' , 'FreeBusyInfo' , 
			), 111, (111, (), [ (7, 1, None, None) , (3, 1, None, None) , (12, 17, None, None) , (16392, 10, None, None) , ], 1 , 1 , 4 , 1 , 116 , (3, 0, None, None) , 0 , )),
	(( 'Resolve' , 'Success' , ), 113, (113, (), [ (16395, 10, None, None) , ], 1 , 1 , 4 , 0 , 120 , (3, 0, None, None) , 0 , )),
] Recipients_vtables_dispatch_ = 1 Recipients_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, "IID('{00063045-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Add' , 'Name' , 'Recipient' , ), 111, (111, (), [ (8, 1, None, None) , 
			(16393, 10, None, "IID('{00063045-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Remove' , 'Index' , ), 84, (84, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'ResolveAll' , 'Success' , ), 126, (126, (), [ (16395, 10, None, None) , ], 1 , 1 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
] RecurrencePattern_vtables_dispatch_ = 1 RecurrencePattern_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'DayOfMonth' , 'DayOfMonth' , ), 4096, (4096, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'DayOfMonth' , 'DayOfMonth' , ), 4096, (4096, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'DayOfWeekMask' , 'DayOfWeekMask' , ), 4097, (4097, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'DayOfWeekMask' , 'DayOfWeekMask' , ), 4097, (4097, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Duration' , 'Duration' , ), 4109, (4109, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Duration' , 'Duration' , ), 4109, (4109, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'EndTime' , 'EndTime' , ), 4108, (4108, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'EndTime' , 'EndTime' , ), 4108, (4108, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Exceptions' , 'Exceptions' , ), 4110, (4110, (), [ (16393, 10, None, "IID('{0006304C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Instance' , 'Instance' , ), 4099, (4099, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'Instance' , 'Instance' , ), 4099, (4099, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'Interval' , 'Interval' , ), 4100, (4100, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'Interval' , 'Interval' , ), 4100, (4100, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'MonthOfYear' , 'MonthOfYear' , ), 4102, (4102, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'MonthOfYear' , 'MonthOfYear' , ), 4102, (4102, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'NoEndDate' , 'NoEndDate' , ), 4107, (4107, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'NoEndDate' , 'NoEndDate' , ), 4107, (4107, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Occurrences' , 'Occurrences' , ), 4101, (4101, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'Occurrences' , 'Occurrences' , ), 4101, (4101, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'PatternEndDate' , 'PatternEndDate' , ), 4098, (4098, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 0 , )),
	(( 'PatternEndDate' , 'PatternEndDate' , ), 4098, (4098, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'PatternStartDate' , 'PatternStartDate' , ), 4104, (4104, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'PatternStartDate' , 'PatternStartDate' , ), 4104, (4104, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'RecurrenceType' , 'RecurrenceType' , ), 4103, (4103, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'RecurrenceType' , 'RecurrenceType' , ), 4103, (4103, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'Regenerate' , 'Regenerate' , ), 4106, (4106, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'Regenerate' , 'Regenerate' , ), 4106, (4106, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'StartTime' , 'StartTime' , ), 4105, (4105, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'StartTime' , 'StartTime' , ), 4105, (4105, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'GetOccurrence' , 'StartDate' , 'AppointmentItem' , ), 4111, (4111, (), [ (7, 1, None, None) , 
			(16397, 10, None, "IID('{00061030-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
] Selection_vtables_dispatch_ = 1 Selection_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
] SyncObjects_vtables_dispatch_ = 1 SyncObjects_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16397, 10, None, "IID('{00063084-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
] UserProperties_vtables_dispatch_ = 1 UserProperties_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, "IID('{00063042-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Add' , 'Name' , 'Type' , 'AddToFolderFields' , 'DisplayFormat' , 
			'UserProperty' , ), 102, (102, (), [ (8, 1, None, None) , (3, 1, None, None) , (12, 17, None, None) , 
			(12, 17, None, None) , (16393, 10, None, "IID('{00063042-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 2 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Find' , 'Name' , 'Custom' , 'UserProperty' , ), 103, (103, (), [ 
			(8, 1, None, None) , (12, 17, None, None) , (16393, 10, None, "IID('{00063042-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 1 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Remove' , 'Index' , ), 82, (82, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
] UserProperty_vtables_dispatch_ = 1 UserProperty_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Formula' , 'Formula' , ), 103, (103, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Formula' , 'Formula' , ), 103, (103, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Type' , 'Type' , ), 109, (109, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'ValidationFormula' , 'ValidationFormula' , ), 104, (104, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'ValidationFormula' , 'ValidationFormula' , ), 104, (104, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'ValidationText' , 'ValidationText' , ), 105, (105, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'ValidationText' , 'ValidationText' , ), 105, (105, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Value' , 'Value' , ), 0, (0, (), [ (16396, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Value' , 'Value' , ), 0, (0, (), [ (12, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 108, (108, (), [ ], 1 , 1 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
] _Application_vtables_dispatch_ = 1 _Application_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Assistant' , 'Assistant' , ), 276, (276, (), [ (16393, 10, None, "IID('{000C0322-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 12289, (12289, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Version' , 'Version' , ), 278, (278, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'ActiveExplorer' , 'ActiveExplorer' , ), 273, (273, (), [ (16393, 10, None, "IID('{00063003-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'ActiveInspector' , 'ActiveInspector' , ), 274, (274, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'CreateItem' , 'ItemType' , 'Item' , ), 266, (266, (), [ (3, 1, None, None) , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'CreateItemFromTemplate' , 'TemplatePath' , 'InFolder' , 'Item' , ), 267, (267, (), [ 
			(8, 1, None, None) , (12, 17, None, None) , (16393, 10, None, None) , ], 1 , 1 , 4 , 1 , 68 , (3, 0, None, None) , 0 , )),
	(( 'CreateObject' , 'ObjectName' , 'Object' , ), 277, (277, (), [ (8, 1, None, None) , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'GetNamespace' , 'Type' , 'NameSpace' , ), 272, (272, (), [ (8, 1, None, None) , 
			(16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Quit' , ), 275, (275, (), [ ], 1 , 1 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'COMAddIns' , 'COMAddIns' , ), 280, (280, (), [ (16393, 10, None, "IID('{000C0339-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'Explorers' , 'Explorers' , ), 281, (281, (), [ (16393, 10, None, "IID('{0006300A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'Inspectors' , 'Inspectors' , ), 282, (282, (), [ (16393, 10, None, "IID('{00063008-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'LanguageSettings' , 'LanguageSettings' , ), 283, (283, (), [ (16393, 10, None, "IID('{000C0353-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'ProductCode' , 'ProductCode' , ), 284, (284, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'AnswerWizard' , 'AnswerWizard' , ), 285, (285, (), [ (16393, 10, None, "IID('{000C0360-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'FeatureInstall' , 'FeatureInstall' , ), 286, (286, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 64 , )),
	(( 'FeatureInstall' , 'FeatureInstall' , ), 286, (286, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 64 , )),
	(( 'ActiveWindow' , 'ActiveWindow' , ), 287, (287, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
] _AppointmentItem_vtables_dispatch_ = 1 _AppointmentItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'AllDayEvent' , 'AllDayEvent' , ), 33301, (33301, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
	(( 'AllDayEvent' , 'AllDayEvent' , ), 33301, (33301, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 228 , (3, 0, None, None) , 0 , )),
	(( 'BusyStatus' , 'BusyStatus' , ), 33285, (33285, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 232 , (3, 0, None, None) , 0 , )),
	(( 'BusyStatus' , 'BusyStatus' , ), 33285, (33285, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 236 , (3, 0, None, None) , 0 , )),
	(( 'Duration' , 'Duration' , ), 33299, (33299, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 240 , (3, 0, None, None) , 0 , )),
	(( 'Duration' , 'Duration' , ), 33299, (33299, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 244 , (3, 0, None, None) , 0 , )),
	(( 'End' , 'End' , ), 33294, (33294, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 248 , (3, 0, None, None) , 0 , )),
	(( 'End' , 'End' , ), 33294, (33294, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 252 , (3, 0, None, None) , 0 , )),
	(( 'IsOnlineMeeting' , 'IsOnlineMeeting' , ), 33344, (33344, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 256 , (3, 0, None, None) , 0 , )),
	(( 'IsOnlineMeeting' , 'IsOnlineMeeting' , ), 33344, (33344, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 260 , (3, 0, None, None) , 0 , )),
	(( 'IsRecurring' , 'IsRecurring' , ), 33315, (33315, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 264 , (3, 0, None, None) , 0 , )),
	(( 'Location' , 'Location' , ), 33288, (33288, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 268 , (3, 0, None, None) , 0 , )),
	(( 'Location' , 'Location' , ), 33288, (33288, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 272 , (3, 0, None, None) , 0 , )),
	(( 'MeetingStatus' , 'MeetingStatus' , ), 33303, (33303, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 276 , (3, 0, None, None) , 0 , )),
	(( 'MeetingStatus' , 'MeetingStatus' , ), 33303, (33303, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 280 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingAutoStart' , 'NetMeetingAutoStart' , ), 33348, (33348, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 284 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingAutoStart' , 'NetMeetingAutoStart' , ), 33348, (33348, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 288 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingOrganizerAlias' , 'NetMeetingOrganizerAlias' , ), 33347, (33347, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 292 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingOrganizerAlias' , 'NetMeetingOrganizerAlias' , ), 33347, (33347, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 296 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingServer' , 'NetMeetingServer' , ), 33346, (33346, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 300 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingServer' , 'NetMeetingServer' , ), 33346, (33346, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 304 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingType' , 'NetMeetingType' , ), 33345, (33345, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 308 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingType' , 'NetMeetingType' , ), 33345, (33345, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 312 , (3, 0, None, None) , 0 , )),
	(( 'OptionalAttendees' , 'OptionalAttendees' , ), 3587, (3587, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 316 , (3, 0, None, None) , 0 , )),
	(( 'OptionalAttendees' , 'OptionalAttendees' , ), 3587, (3587, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 320 , (3, 0, None, None) , 0 , )),
	(( 'Organizer' , 'Organizer' , ), 66, (66, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 324 , (3, 0, None, None) , 0 , )),
	(( 'Recipients' , 'Recipients' , ), 63508, (63508, (), [ (16393, 10, None, "IID('{0006303B-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 328 , (3, 0, None, None) , 0 , )),
	(( 'RecurrenceState' , 'RecurrenceState' , ), 62789, (62789, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 332 , (3, 0, None, None) , 0 , )),
	(( 'ReminderMinutesBeforeStart' , 'ReminderMinutesBeforeStart' , ), 34049, (34049, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 336 , (3, 0, None, None) , 0 , )),
	(( 'ReminderMinutesBeforeStart' , 'ReminderMinutesBeforeStart' , ), 34049, (34049, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 340 , (3, 0, None, None) , 0 , )),
	(( 'ReminderOverrideDefault' , 'ReminderOverrideDefault' , ), 34076, (34076, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 344 , (3, 0, None, None) , 0 , )),
	(( 'ReminderOverrideDefault' , 'ReminderOverrideDefault' , ), 34076, (34076, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 348 , (3, 0, None, None) , 0 , )),
	(( 'ReminderPlaySound' , 'ReminderPlaySound' , ), 34078, (34078, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 352 , (3, 0, None, None) , 0 , )),
	(( 'ReminderPlaySound' , 'ReminderPlaySound' , ), 34078, (34078, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 356 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSet' , 'ReminderSet' , ), 34051, (34051, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 360 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSet' , 'ReminderSet' , ), 34051, (34051, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 364 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSoundFile' , 'ReminderSoundFile' , ), 34079, (34079, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 368 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSoundFile' , 'ReminderSoundFile' , ), 34079, (34079, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 372 , (3, 0, None, None) , 0 , )),
	(( 'ReplyTime' , 'ReplyTime' , ), 33312, (33312, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 376 , (3, 0, None, None) , 0 , )),
	(( 'ReplyTime' , 'ReplyTime' , ), 33312, (33312, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 380 , (3, 0, None, None) , 0 , )),
	(( 'RequiredAttendees' , 'RequiredAttendees' , ), 3588, (3588, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 384 , (3, 0, None, None) , 0 , )),
	(( 'RequiredAttendees' , 'RequiredAttendees' , ), 3588, (3588, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 388 , (3, 0, None, None) , 0 , )),
	(( 'Resources' , 'Resources' , ), 3586, (3586, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 392 , (3, 0, None, None) , 0 , )),
	(( 'Resources' , 'Resources' , ), 3586, (3586, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 396 , (3, 0, None, None) , 0 , )),
	(( 'ResponseRequested' , 'ResponseRequested' , ), 99, (99, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 400 , (3, 0, None, None) , 0 , )),
	(( 'ResponseRequested' , 'ResponseRequested' , ), 99, (99, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 404 , (3, 0, None, None) , 0 , )),
	(( 'ResponseStatus' , 'ResponseStatus' , ), 33304, (33304, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 408 , (3, 0, None, None) , 0 , )),
	(( 'Start' , 'Start' , ), 33293, (33293, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 412 , (3, 0, None, None) , 0 , )),
	(( 'Start' , 'Start' , ), 33293, (33293, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 416 , (3, 0, None, None) , 0 , )),
	(( 'ClearRecurrencePattern' , ), 61605, (61605, (), [ ], 1 , 1 , 4 , 0 , 420 , (3, 0, None, None) , 0 , )),
	(( 'ForwardAsVcal' , 'Item' , ), 62791, (62791, (), [ (16397, 10, None, "IID('{00061033-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 424 , (3, 0, None, None) , 0 , )),
	(( 'GetRecurrencePattern' , 'RecurrencPattern' , ), 61604, (61604, (), [ (16393, 10, None, "IID('{00063044-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 428 , (3, 0, None, None) , 0 , )),
	(( 'Respond' , 'Response' , 'fNoUI' , 'fAdditionalTextDialog' , 'ResponseItem' , 
			), 62722, (62722, (), [ (3, 1, None, None) , (12, 17, None, None) , (12, 17, None, None) , (16397, 10, None, "IID('{00061036-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 2 , 432 , (3, 0, None, None) , 0 , )),
	(( 'Send' , ), 61557, (61557, (), [ ], 1 , 1 , 4 , 0 , 436 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingDocPathName' , 'NetMeetingDocPathName' , ), 33351, (33351, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 440 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingDocPathName' , 'NetMeetingDocPathName' , ), 33351, (33351, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 444 , (3, 0, None, None) , 0 , )),
	(( 'NetShowURL' , 'NetShowURL' , ), 33352, (33352, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 448 , (3, 0, None, None) , 0 , )),
	(( 'NetShowURL' , 'NetShowURL' , ), 33352, (33352, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 452 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 456 , (3, 0, None, None) , 0 , )),
	(( 'ConferenceServerAllowExternal' , 'ConferenceServerAllowExternal' , ), 33350, (33350, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 460 , (3, 0, None, None) , 0 , )),
	(( 'ConferenceServerAllowExternal' , 'ConferenceServerAllowExternal' , ), 33350, (33350, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 464 , (3, 0, None, None) , 0 , )),
	(( 'ConferenceServerPassword' , 'ConferenceServerPassword' , ), 33353, (33353, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 468 , (3, 0, None, None) , 0 , )),
	(( 'ConferenceServerPassword' , 'ConferenceServerPassword' , ), 33353, (33353, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 472 , (3, 0, None, None) , 0 , )),
] _ContactItem_vtables_dispatch_ = 1 _ContactItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'Account' , 'Account' , ), 14848, (14848, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
	(( 'Account' , 'Account' , ), 14848, (14848, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 228 , (3, 0, None, None) , 0 , )),
	(( 'Anniversary' , 'Anniversary' , ), 14913, (14913, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 232 , (3, 0, None, None) , 0 , )),
	(( 'Anniversary' , 'Anniversary' , ), 14913, (14913, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 236 , (3, 0, None, None) , 0 , )),
	(( 'AssistantName' , 'AssistantName' , ), 14896, (14896, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 240 , (3, 0, None, None) , 0 , )),
	(( 'AssistantName' , 'AssistantName' , ), 14896, (14896, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 244 , (3, 0, None, None) , 0 , )),
	(( 'AssistantTelephoneNumber' , 'AssistantTelephoneNumber' , ), 14894, (14894, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 248 , (3, 0, None, None) , 0 , )),
	(( 'AssistantTelephoneNumber' , 'AssistantTelephoneNumber' , ), 14894, (14894, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 252 , (3, 0, None, None) , 0 , )),
	(( 'Birthday' , 'Birthday' , ), 14914, (14914, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 256 , (3, 0, None, None) , 0 , )),
	(( 'Birthday' , 'Birthday' , ), 14914, (14914, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 260 , (3, 0, None, None) , 0 , )),
	(( 'Business2TelephoneNumber' , 'Business2TelephoneNumber' , ), 14875, (14875, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 264 , (3, 0, None, None) , 0 , )),
	(( 'Business2TelephoneNumber' , 'Business2TelephoneNumber' , ), 14875, (14875, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 268 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddress' , 'BusinessAddress' , ), 32795, (32795, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 272 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddress' , 'BusinessAddress' , ), 32795, (32795, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 276 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddressCity' , 'BusinessAddressCity' , ), 32838, (32838, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 280 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddressCity' , 'BusinessAddressCity' , ), 32838, (32838, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 284 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddressCountry' , 'BusinessAddressCountry' , ), 32841, (32841, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 288 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddressCountry' , 'BusinessAddressCountry' , ), 32841, (32841, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 292 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddressPostalCode' , 'BusinessAddressPostalCode' , ), 32840, (32840, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 296 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddressPostalCode' , 'BusinessAddressPostalCode' , ), 32840, (32840, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 300 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddressPostOfficeBox' , 'BusinessAddressPostOfficeBox' , ), 32842, (32842, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 304 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddressPostOfficeBox' , 'BusinessAddressPostOfficeBox' , ), 32842, (32842, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 308 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddressState' , 'BusinessAddressState' , ), 32839, (32839, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 312 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddressState' , 'BusinessAddressState' , ), 32839, (32839, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 316 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddressStreet' , 'BusinessAddressStreet' , ), 32837, (32837, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 320 , (3, 0, None, None) , 0 , )),
	(( 'BusinessAddressStreet' , 'BusinessAddressStreet' , ), 32837, (32837, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 324 , (3, 0, None, None) , 0 , )),
	(( 'BusinessFaxNumber' , 'BusinessFaxNumber' , ), 14884, (14884, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 328 , (3, 0, None, None) , 0 , )),
	(( 'BusinessFaxNumber' , 'BusinessFaxNumber' , ), 14884, (14884, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 332 , (3, 0, None, None) , 0 , )),
	(( 'BusinessHomePage' , 'BusinessHomePage' , ), 14929, (14929, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 336 , (3, 0, None, None) , 0 , )),
	(( 'BusinessHomePage' , 'BusinessHomePage' , ), 14929, (14929, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 340 , (3, 0, None, None) , 0 , )),
	(( 'BusinessTelephoneNumber' , 'BusinessTelephoneNumber' , ), 14856, (14856, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 344 , (3, 0, None, None) , 0 , )),
	(( 'BusinessTelephoneNumber' , 'BusinessTelephoneNumber' , ), 14856, (14856, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 348 , (3, 0, None, None) , 0 , )),
	(( 'CallbackTelephoneNumber' , 'CallbackTelephoneNumber' , ), 14850, (14850, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 352 , (3, 0, None, None) , 0 , )),
	(( 'CallbackTelephoneNumber' , 'CallbackTelephoneNumber' , ), 14850, (14850, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 356 , (3, 0, None, None) , 0 , )),
	(( 'CarTelephoneNumber' , 'CarTelephoneNumber' , ), 14878, (14878, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 360 , (3, 0, None, None) , 0 , )),
	(( 'CarTelephoneNumber' , 'CarTelephoneNumber' , ), 14878, (14878, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 364 , (3, 0, None, None) , 0 , )),
	(( 'Children' , 'Children' , ), 32780, (32780, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 368 , (3, 0, None, None) , 0 , )),
	(( 'Children' , 'Children' , ), 32780, (32780, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 372 , (3, 0, None, None) , 0 , )),
	(( 'CompanyAndFullName' , 'CompanyAndFullName' , ), 32792, (32792, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 376 , (3, 0, None, None) , 0 , )),
	(( 'CompanyLastFirstNoSpace' , 'CompanyLastFirstNoSpace' , ), 32818, (32818, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 380 , (3, 0, None, None) , 0 , )),
	(( 'CompanyLastFirstSpaceOnly' , 'CompanyLastFirstSpaceOnly' , ), 32819, (32819, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 384 , (3, 0, None, None) , 0 , )),
	(( 'CompanyMainTelephoneNumber' , 'CompanyMainTelephoneNumber' , ), 14935, (14935, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 388 , (3, 0, None, None) , 0 , )),
	(( 'CompanyMainTelephoneNumber' , 'CompanyMainTelephoneNumber' , ), 14935, (14935, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 392 , (3, 0, None, None) , 0 , )),
	(( 'CompanyName' , 'CompanyName' , ), 14870, (14870, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 396 , (3, 0, None, None) , 0 , )),
	(( 'CompanyName' , 'CompanyName' , ), 14870, (14870, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 400 , (3, 0, None, None) , 0 , )),
	(( 'ComputerNetworkName' , 'ComputerNetworkName' , ), 14921, (14921, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 404 , (3, 0, None, None) , 0 , )),
	(( 'ComputerNetworkName' , 'ComputerNetworkName' , ), 14921, (14921, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 408 , (3, 0, None, None) , 0 , )),
	(( 'CustomerID' , 'CustomerID' , ), 14922, (14922, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 412 , (3, 0, None, None) , 0 , )),
	(( 'CustomerID' , 'CustomerID' , ), 14922, (14922, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 416 , (3, 0, None, None) , 0 , )),
	(( 'Department' , 'Department' , ), 14872, (14872, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 420 , (3, 0, None, None) , 0 , )),
	(( 'Department' , 'Department' , ), 14872, (14872, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 424 , (3, 0, None, None) , 0 , )),
	(( 'Email1Address' , 'Email1Address' , ), 32899, (32899, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 428 , (3, 0, None, None) , 0 , )),
	(( 'Email1Address' , 'Email1Address' , ), 32899, (32899, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 432 , (3, 0, None, None) , 0 , )),
	(( 'Email1AddressType' , 'Email1AddressType' , ), 32898, (32898, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 436 , (3, 0, None, None) , 0 , )),
	(( 'Email1AddressType' , 'Email1AddressType' , ), 32898, (32898, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 440 , (3, 0, None, None) , 0 , )),
	(( 'Email1DisplayName' , 'Email1DisplayName' , ), 32896, (32896, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 444 , (3, 0, None, None) , 0 , )),
	(( 'Email1EntryID' , 'Email1EntryID' , ), 32901, (32901, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 448 , (3, 0, None, None) , 0 , )),
	(( 'Email2Address' , 'Email2Address' , ), 32915, (32915, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 452 , (3, 0, None, None) , 0 , )),
	(( 'Email2Address' , 'Email2Address' , ), 32915, (32915, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 456 , (3, 0, None, None) , 0 , )),
	(( 'Email2AddressType' , 'Email2AddressType' , ), 32914, (32914, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 460 , (3, 0, None, None) , 0 , )),
	(( 'Email2AddressType' , 'Email2AddressType' , ), 32914, (32914, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 464 , (3, 0, None, None) , 0 , )),
	(( 'Email2DisplayName' , 'Email2DisplayName' , ), 32912, (32912, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 468 , (3, 0, None, None) , 0 , )),
	(( 'Email2EntryID' , 'Email2EntryID' , ), 32917, (32917, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 472 , (3, 0, None, None) , 0 , )),
	(( 'Email3Address' , 'Email3Address' , ), 32931, (32931, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 476 , (3, 0, None, None) , 0 , )),
	(( 'Email3Address' , 'Email3Address' , ), 32931, (32931, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 480 , (3, 0, None, None) , 0 , )),
	(( 'Email3AddressType' , 'Email3AddressType' , ), 32930, (32930, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 484 , (3, 0, None, None) , 0 , )),
	(( 'Email3AddressType' , 'Email3AddressType' , ), 32930, (32930, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 488 , (3, 0, None, None) , 0 , )),
	(( 'Email3DisplayName' , 'Email3DisplayName' , ), 32928, (32928, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 492 , (3, 0, None, None) , 0 , )),
	(( 'Email3EntryID' , 'Email3EntryID' , ), 32933, (32933, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 496 , (3, 0, None, None) , 0 , )),
	(( 'FileAs' , 'FileAs' , ), 32773, (32773, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 500 , (3, 0, None, None) , 0 , )),
	(( 'FileAs' , 'FileAs' , ), 32773, (32773, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 504 , (3, 0, None, None) , 0 , )),
	(( 'FirstName' , 'FirstName' , ), 14854, (14854, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 508 , (3, 0, None, None) , 0 , )),
	(( 'FirstName' , 'FirstName' , ), 14854, (14854, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 512 , (3, 0, None, None) , 0 , )),
	(( 'FTPSite' , 'FTPSite' , ), 14924, (14924, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 516 , (3, 0, None, None) , 0 , )),
	(( 'FTPSite' , 'FTPSite' , ), 14924, (14924, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 520 , (3, 0, None, None) , 0 , )),
	(( 'FullName' , 'FullName' , ), 12289, (12289, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 524 , (3, 0, None, None) , 0 , )),
	(( 'FullName' , 'FullName' , ), 12289, (12289, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 528 , (3, 0, None, None) , 0 , )),
	(( 'FullNameAndCompany' , 'FullNameAndCompany' , ), 32793, (32793, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 532 , (3, 0, None, None) , 0 , )),
	(( 'Gender' , 'Gender' , ), 14925, (14925, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 536 , (3, 0, None, None) , 0 , )),
	(( 'Gender' , 'Gender' , ), 14925, (14925, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 540 , (3, 0, None, None) , 0 , )),
	(( 'GovernmentIDNumber' , 'GovernmentIDNumber' , ), 14855, (14855, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 544 , (3, 0, None, None) , 0 , )),
	(( 'GovernmentIDNumber' , 'GovernmentIDNumber' , ), 14855, (14855, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 548 , (3, 0, None, None) , 0 , )),
	(( 'Hobby' , 'Hobby' , ), 14915, (14915, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 552 , (3, 0, None, None) , 0 , )),
	(( 'Hobby' , 'Hobby' , ), 14915, (14915, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 556 , (3, 0, None, None) , 0 , )),
	(( 'Home2TelephoneNumber' , 'Home2TelephoneNumber' , ), 14895, (14895, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 560 , (3, 0, None, None) , 0 , )),
	(( 'Home2TelephoneNumber' , 'Home2TelephoneNumber' , ), 14895, (14895, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 564 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddress' , 'HomeAddress' , ), 32794, (32794, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 568 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddress' , 'HomeAddress' , ), 32794, (32794, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 572 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddressCity' , 'HomeAddressCity' , ), 14937, (14937, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 576 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddressCity' , 'HomeAddressCity' , ), 14937, (14937, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 580 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddressCountry' , 'HomeAddressCountry' , ), 14938, (14938, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 584 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddressCountry' , 'HomeAddressCountry' , ), 14938, (14938, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 588 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddressPostalCode' , 'HomeAddressPostalCode' , ), 14939, (14939, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 592 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddressPostalCode' , 'HomeAddressPostalCode' , ), 14939, (14939, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 596 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddressPostOfficeBox' , 'HomeAddressPostOfficeBox' , ), 14942, (14942, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 600 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddressPostOfficeBox' , 'HomeAddressPostOfficeBox' , ), 14942, (14942, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 604 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddressState' , 'HomeAddressState' , ), 14940, (14940, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 608 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddressState' , 'HomeAddressState' , ), 14940, (14940, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 612 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddressStreet' , 'HomeAddressStreet' , ), 14941, (14941, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 616 , (3, 0, None, None) , 0 , )),
	(( 'HomeAddressStreet' , 'HomeAddressStreet' , ), 14941, (14941, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 620 , (3, 0, None, None) , 0 , )),
	(( 'HomeFaxNumber' , 'HomeFaxNumber' , ), 14885, (14885, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 624 , (3, 0, None, None) , 0 , )),
	(( 'HomeFaxNumber' , 'HomeFaxNumber' , ), 14885, (14885, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 628 , (3, 0, None, None) , 0 , )),
	(( 'HomeTelephoneNumber' , 'HomeTelephoneNumber' , ), 14857, (14857, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 632 , (3, 0, None, None) , 0 , )),
	(( 'HomeTelephoneNumber' , 'HomeTelephoneNumber' , ), 14857, (14857, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 636 , (3, 0, None, None) , 0 , )),
	(( 'Initials' , 'Initials' , ), 14858, (14858, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 640 , (3, 0, None, None) , 0 , )),
	(( 'Initials' , 'Initials' , ), 14858, (14858, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 644 , (3, 0, None, None) , 0 , )),
	(( 'InternetFreeBusyAddress' , 'InternetFreeBusyAddress' , ), 32984, (32984, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 648 , (3, 0, None, None) , 0 , )),
	(( 'InternetFreeBusyAddress' , 'InternetFreeBusyAddress' , ), 32984, (32984, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 652 , (3, 0, None, None) , 0 , )),
	(( 'ISDNNumber' , 'ISDNNumber' , ), 14893, (14893, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 656 , (3, 0, None, None) , 0 , )),
	(( 'ISDNNumber' , 'ISDNNumber' , ), 14893, (14893, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 660 , (3, 0, None, None) , 0 , )),
	(( 'JobTitle' , 'JobTitle' , ), 14871, (14871, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 664 , (3, 0, None, None) , 0 , )),
	(( 'JobTitle' , 'JobTitle' , ), 14871, (14871, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 668 , (3, 0, None, None) , 0 , )),
	(( 'Journal' , 'Journal' , ), 32805, (32805, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 672 , (3, 0, None, None) , 0 , )),
	(( 'Journal' , 'Journal' , ), 32805, (32805, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 676 , (3, 0, None, None) , 0 , )),
	(( 'Language' , 'Language' , ), 14860, (14860, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 680 , (3, 0, None, None) , 0 , )),
	(( 'Language' , 'Language' , ), 14860, (14860, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 684 , (3, 0, None, None) , 0 , )),
	(( 'LastFirstAndSuffix' , 'LastFirstAndSuffix' , ), 32822, (32822, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 688 , (3, 0, None, None) , 0 , )),
	(( 'LastFirstNoSpace' , 'LastFirstNoSpace' , ), 32816, (32816, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 692 , (3, 0, None, None) , 0 , )),
	(( 'LastFirstNoSpaceCompany' , 'LastFirstNoSpaceCompany' , ), 32820, (32820, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 696 , (3, 0, None, None) , 0 , )),
	(( 'LastFirstSpaceOnly' , 'LastFirstSpaceOnly' , ), 32817, (32817, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 700 , (3, 0, None, None) , 0 , )),
	(( 'LastFirstSpaceOnlyCompany' , 'LastFirstSpaceOnlyCompany' , ), 32821, (32821, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 704 , (3, 0, None, None) , 0 , )),
	(( 'LastName' , 'LastName' , ), 14865, (14865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 708 , (3, 0, None, None) , 0 , )),
	(( 'LastName' , 'LastName' , ), 14865, (14865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 712 , (3, 0, None, None) , 0 , )),
	(( 'LastNameAndFirstName' , 'LastNameAndFirstName' , ), 32791, (32791, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 716 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddress' , 'MailingAddress' , ), 14869, (14869, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 720 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddress' , 'MailingAddress' , ), 14869, (14869, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 724 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddressCity' , 'MailingAddressCity' , ), 14887, (14887, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 728 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddressCity' , 'MailingAddressCity' , ), 14887, (14887, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 732 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddressCountry' , 'MailingAddressCountry' , ), 14886, (14886, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 736 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddressCountry' , 'MailingAddressCountry' , ), 14886, (14886, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 740 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddressPostalCode' , 'MailingAddressPostalCode' , ), 14890, (14890, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 744 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddressPostalCode' , 'MailingAddressPostalCode' , ), 14890, (14890, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 748 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddressPostOfficeBox' , 'MailingAddressPostOfficeBox' , ), 14891, (14891, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 752 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddressPostOfficeBox' , 'MailingAddressPostOfficeBox' , ), 14891, (14891, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 756 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddressState' , 'MailingAddressState' , ), 14888, (14888, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 760 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddressState' , 'MailingAddressState' , ), 14888, (14888, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 764 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddressStreet' , 'MailingAddressStreet' , ), 14889, (14889, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 768 , (3, 0, None, None) , 0 , )),
	(( 'MailingAddressStreet' , 'MailingAddressStreet' , ), 14889, (14889, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 772 , (3, 0, None, None) , 0 , )),
	(( 'ManagerName' , 'ManagerName' , ), 14926, (14926, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 776 , (3, 0, None, None) , 0 , )),
	(( 'ManagerName' , 'ManagerName' , ), 14926, (14926, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 780 , (3, 0, None, None) , 0 , )),
	(( 'MiddleName' , 'MiddleName' , ), 14916, (14916, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 784 , (3, 0, None, None) , 0 , )),
	(( 'MiddleName' , 'MiddleName' , ), 14916, (14916, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 788 , (3, 0, None, None) , 0 , )),
	(( 'MobileTelephoneNumber' , 'MobileTelephoneNumber' , ), 14876, (14876, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 792 , (3, 0, None, None) , 0 , )),
	(( 'MobileTelephoneNumber' , 'MobileTelephoneNumber' , ), 14876, (14876, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 796 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingAlias' , 'NetMeetingAlias' , ), 32863, (32863, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 800 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingAlias' , 'NetMeetingAlias' , ), 32863, (32863, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 804 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingServer' , 'NetMeetingServer' , ), 32864, (32864, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 808 , (3, 0, None, None) , 0 , )),
	(( 'NetMeetingServer' , 'NetMeetingServer' , ), 32864, (32864, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 812 , (3, 0, None, None) , 0 , )),
	(( 'NickName' , 'NickName' , ), 14927, (14927, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 816 , (3, 0, None, None) , 0 , )),
	(( 'NickName' , 'NickName' , ), 14927, (14927, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 820 , (3, 0, None, None) , 0 , )),
	(( 'OfficeLocation' , 'OfficeLocation' , ), 14873, (14873, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 824 , (3, 0, None, None) , 0 , )),
	(( 'OfficeLocation' , 'OfficeLocation' , ), 14873, (14873, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 828 , (3, 0, None, None) , 0 , )),
	(( 'OrganizationalIDNumber' , 'OrganizationalIDNumber' , ), 14864, (14864, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 832 , (3, 0, None, None) , 0 , )),
	(( 'OrganizationalIDNumber' , 'OrganizationalIDNumber' , ), 14864, (14864, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 836 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddress' , 'OtherAddress' , ), 32796, (32796, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 840 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddress' , 'OtherAddress' , ), 32796, (32796, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 844 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddressCity' , 'OtherAddressCity' , ), 14943, (14943, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 848 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddressCity' , 'OtherAddressCity' , ), 14943, (14943, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 852 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddressCountry' , 'OtherAddressCountry' , ), 14944, (14944, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 856 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddressCountry' , 'OtherAddressCountry' , ), 14944, (14944, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 860 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddressPostalCode' , 'OtherAddressPostalCode' , ), 14945, (14945, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 864 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddressPostalCode' , 'OtherAddressPostalCode' , ), 14945, (14945, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 868 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddressPostOfficeBox' , 'OtherAddressPostOfficeBox' , ), 14948, (14948, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 872 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddressPostOfficeBox' , 'OtherAddressPostOfficeBox' , ), 14948, (14948, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 876 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddressState' , 'OtherAddressState' , ), 14946, (14946, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 880 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddressState' , 'OtherAddressState' , ), 14946, (14946, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 884 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddressStreet' , 'OtherAddressStreet' , ), 14947, (14947, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 888 , (3, 0, None, None) , 0 , )),
	(( 'OtherAddressStreet' , 'OtherAddressStreet' , ), 14947, (14947, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 892 , (3, 0, None, None) , 0 , )),
	(( 'OtherFaxNumber' , 'OtherFaxNumber' , ), 14883, (14883, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 896 , (3, 0, None, None) , 0 , )),
	(( 'OtherFaxNumber' , 'OtherFaxNumber' , ), 14883, (14883, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 900 , (3, 0, None, None) , 0 , )),
	(( 'OtherTelephoneNumber' , 'OtherTelephoneNumber' , ), 14879, (14879, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 904 , (3, 0, None, None) , 0 , )),
	(( 'OtherTelephoneNumber' , 'OtherTelephoneNumber' , ), 14879, (14879, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 908 , (3, 0, None, None) , 0 , )),
	(( 'PagerNumber' , 'PagerNumber' , ), 14881, (14881, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 912 , (3, 0, None, None) , 0 , )),
	(( 'PagerNumber' , 'PagerNumber' , ), 14881, (14881, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 916 , (3, 0, None, None) , 0 , )),
	(( 'PersonalHomePage' , 'PersonalHomePage' , ), 14928, (14928, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 920 , (3, 0, None, None) , 0 , )),
	(( 'PersonalHomePage' , 'PersonalHomePage' , ), 14928, (14928, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 924 , (3, 0, None, None) , 0 , )),
	(( 'PrimaryTelephoneNumber' , 'PrimaryTelephoneNumber' , ), 14874, (14874, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 928 , (3, 0, None, None) , 0 , )),
	(( 'PrimaryTelephoneNumber' , 'PrimaryTelephoneNumber' , ), 14874, (14874, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 932 , (3, 0, None, None) , 0 , )),
	(( 'Profession' , 'Profession' , ), 14918, (14918, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 936 , (3, 0, None, None) , 0 , )),
	(( 'Profession' , 'Profession' , ), 14918, (14918, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 940 , (3, 0, None, None) , 0 , )),
	(( 'RadioTelephoneNumber' , 'RadioTelephoneNumber' , ), 14877, (14877, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 944 , (3, 0, None, None) , 0 , )),
	(( 'RadioTelephoneNumber' , 'RadioTelephoneNumber' , ), 14877, (14877, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 948 , (3, 0, None, None) , 0 , )),
	(( 'ReferredBy' , 'ReferredBy' , ), 14919, (14919, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 952 , (3, 0, None, None) , 0 , )),
	(( 'ReferredBy' , 'ReferredBy' , ), 14919, (14919, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 956 , (3, 0, None, None) , 0 , )),
	(( 'SelectedMailingAddress' , 'SelectedMailingAddress' , ), 32802, (32802, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 960 , (3, 0, None, None) , 0 , )),
	(( 'SelectedMailingAddress' , 'SelectedMailingAddress' , ), 32802, (32802, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 964 , (3, 0, None, None) , 0 , )),
	(( 'Spouse' , 'Spouse' , ), 14920, (14920, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 968 , (3, 0, None, None) , 0 , )),
	(( 'Spouse' , 'Spouse' , ), 14920, (14920, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 972 , (3, 0, None, None) , 0 , )),
	(( 'Suffix' , 'Suffix' , ), 14853, (14853, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 976 , (3, 0, None, None) , 0 , )),
	(( 'Suffix' , 'Suffix' , ), 14853, (14853, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 980 , (3, 0, None, None) , 0 , )),
	(( 'TelexNumber' , 'TelexNumber' , ), 14892, (14892, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 984 , (3, 0, None, None) , 0 , )),
	(( 'TelexNumber' , 'TelexNumber' , ), 14892, (14892, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 988 , (3, 0, None, None) , 0 , )),
	(( 'Title' , 'Title' , ), 14917, (14917, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 992 , (3, 0, None, None) , 0 , )),
	(( 'Title' , 'Title' , ), 14917, (14917, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 996 , (3, 0, None, None) , 0 , )),
	(( 'TTYTDDTelephoneNumber' , 'TTYTDDTelephoneNumber' , ), 14923, (14923, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 1000 , (3, 0, None, None) , 0 , )),
	(( 'TTYTDDTelephoneNumber' , 'TTYTDDTelephoneNumber' , ), 14923, (14923, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 1004 , (3, 0, None, None) , 0 , )),
	(( 'User1' , 'User1' , ), 32847, (32847, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 1008 , (3, 0, None, None) , 0 , )),
	(( 'User1' , 'User1' , ), 32847, (32847, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 1012 , (3, 0, None, None) , 0 , )),
	(( 'User2' , 'User2' , ), 32848, (32848, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 1016 , (3, 0, None, None) , 0 , )),
	(( 'User2' , 'User2' , ), 32848, (32848, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 1020 , (3, 0, None, None) , 0 , )),
	(( 'User3' , 'User3' , ), 32849, (32849, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 1024 , (3, 0, None, None) , 0 , )),
	(( 'User3' , 'User3' , ), 32849, (32849, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 1028 , (3, 0, None, None) , 0 , )),
	(( 'User4' , 'User4' , ), 32850, (32850, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 1032 , (3, 0, None, None) , 0 , )),
	(( 'User4' , 'User4' , ), 32850, (32850, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 1036 , (3, 0, None, None) , 0 , )),
	(( 'UserCertificate' , 'UserCertificate' , ), 32790, (32790, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 1040 , (3, 0, None, None) , 0 , )),
	(( 'UserCertificate' , 'UserCertificate' , ), 32790, (32790, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 1044 , (3, 0, None, None) , 0 , )),
	(( 'WebPage' , 'WebPage' , ), 32811, (32811, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 1048 , (3, 0, None, None) , 0 , )),
	(( 'WebPage' , 'WebPage' , ), 32811, (32811, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 1052 , (3, 0, None, None) , 0 , )),
	(( 'YomiCompanyName' , 'YomiCompanyName' , ), 32814, (32814, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 1056 , (3, 0, None, None) , 0 , )),
	(( 'YomiCompanyName' , 'YomiCompanyName' , ), 32814, (32814, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 1060 , (3, 0, None, None) , 0 , )),
	(( 'YomiFirstName' , 'YomiFirstName' , ), 32812, (32812, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 1064 , (3, 0, None, None) , 0 , )),
	(( 'YomiFirstName' , 'YomiFirstName' , ), 32812, (32812, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 1068 , (3, 0, None, None) , 0 , )),
	(( 'YomiLastName' , 'YomiLastName' , ), 32813, (32813, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 1072 , (3, 0, None, None) , 0 , )),
	(( 'YomiLastName' , 'YomiLastName' , ), 32813, (32813, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 1076 , (3, 0, None, None) , 0 , )),
	(( 'ForwardAsVcard' , 'Item' , ), 63649, (63649, (), [ (16397, 10, None, "IID('{00061033-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 1080 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 1084 , (3, 0, None, None) , 0 , )),
] _DistListItem_vtables_dispatch_ = 1 _DistListItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'DLName' , 'DLName' , ), 32851, (32851, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
	(( 'DLName' , 'DLName' , ), 32851, (32851, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 228 , (3, 0, None, None) , 0 , )),
	(( 'MemberCount' , 'MemberCount' , ), 32843, (32843, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 232 , (3, 0, None, None) , 0 , )),
	(( 'CheckSum' , 'CheckSum' , ), 32844, (32844, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 236 , (3, 0, None, None) , 64 , )),
	(( 'Members' , 'Members' , ), 32853, (32853, (), [ (16396, 10, None, None) , ], 1 , 2 , 4 , 0 , 240 , (3, 0, None, None) , 64 , )),
	(( 'Members' , 'Members' , ), 32853, (32853, (), [ (12, 1, None, None) , ], 1 , 4 , 4 , 0 , 244 , (3, 0, None, None) , 64 , )),
	(( 'OneOffMembers' , 'OneOffMembers' , ), 32852, (32852, (), [ (16396, 10, None, None) , ], 1 , 2 , 4 , 0 , 248 , (3, 0, None, None) , 64 , )),
	(( 'OneOffMembers' , 'OneOffMembers' , ), 32852, (32852, (), [ (12, 1, None, None) , ], 1 , 4 , 4 , 0 , 252 , (3, 0, None, None) , 64 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 256 , (3, 0, None, None) , 0 , )),
	(( 'AddMembers' , 'Recipients' , ), 63744, (63744, (), [ (9, 1, None, "IID('{0006303B-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 260 , (3, 0, None, None) , 0 , )),
	(( 'RemoveMembers' , 'Recipients' , ), 63745, (63745, (), [ (9, 1, None, "IID('{0006303B-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 264 , (3, 0, None, None) , 0 , )),
	(( 'GetMember' , 'Index' , 'Recipient' , ), 63749, (63749, (), [ (3, 1, None, None) , 
			(16393, 10, None, "IID('{00063045-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 268 , (3, 0, None, None) , 0 , )),
] _DocumentItem_vtables_dispatch_ = 1 _DocumentItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
] _Explorer_vtables_dispatch_ = 1 _Explorer_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'CommandBars' , 'CommandBars' , ), 8448, (8448, (), [ (16397, 10, None, "IID('{55F88893-7708-11D1-ACEB-006008961DA5}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'CurrentFolder' , 'CurrentFolder' , ), 8449, (8449, (), [ (16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'CurrentFolder' , 'CurrentFolder' , ), 8449, (8449, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 8 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Close' , ), 8451, (8451, (), [ ], 1 , 1 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Display' , ), 8452, (8452, (), [ ], 1 , 1 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Caption' , 'Caption' , ), 8465, (8465, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'CurrentView' , 'CurrentView' , ), 8704, (8704, (), [ (16396, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'CurrentView' , 'CurrentView' , ), 8704, (8704, (), [ (12, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Height' , 'Height' , ), 8468, (8468, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Height' , 'Height' , ), 8468, (8468, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'Left' , 'Left' , ), 8469, (8469, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'Left' , 'Left' , ), 8469, (8469, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'Panes' , 'Panes' , ), 8705, (8705, (), [ (16393, 10, None, "IID('{00063009-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'Selection' , 'Selection' , ), 8706, (8706, (), [ (16393, 10, None, "IID('{00063087-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'Top' , 'Top' , ), 8470, (8470, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'Top' , 'Top' , ), 8470, (8470, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Width' , 'Width' , ), 8471, (8471, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Width' , 'Width' , ), 8471, (8471, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'WindowState' , 'WindowState' , ), 8466, (8466, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'WindowState' , 'WindowState' , ), 8466, (8466, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 120 , (3, 0, None, None) , 0 , )),
	(( 'Activate' , ), 8467, (8467, (), [ ], 1 , 1 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'IsPaneVisible' , 'Pane' , 'IsPaneVisible' , ), 8707, (8707, (), [ (3, 1, None, None) , 
			(16395, 10, None, None) , ], 1 , 1 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'ShowPane' , 'Pane' , 'Visible' , ), 8708, (8708, (), [ (3, 1, None, None) , 
			(11, 1, None, None) , ], 1 , 1 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Views' , 'Views' , ), 12553, (12553, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 136 , (3, 0, None, None) , 64 , )),
] _Explorers_vtables_dispatch_ = 1 _Explorers_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16397, 10, None, "IID('{00063050-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Add' , 'Folder' , 'DisplayMode' , 'Explorer' , ), 95, (95, (), [ 
			(12, 1, None, None) , (3, 17, None, None) , (16393, 10, None, "IID('{00063003-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
] _Folders_vtables_dispatch_ = 1 _Folders_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'RawTable' , 'RawTable' , ), 90, (90, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 64 , )),
	(( 'Add' , 'Name' , 'Type' , 'Folder' , ), 95, (95, (), [ 
			(8, 1, None, None) , (12, 17, None, None) , (16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 1 , 56 , (3, 0, None, None) , 0 , )),
	(( 'GetFirst' , 'Folder' , ), 86, (86, (), [ (16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'GetLast' , 'Folder' , ), 88, (88, (), [ (16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'GetNext' , 'Folder' , ), 87, (87, (), [ (16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'GetPrevious' , 'Folder' , ), 89, (89, (), [ (16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Remove' , 'Index' , ), 84, (84, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
] _Inspector_vtables_dispatch_ = 1 _Inspector_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'CommandBars' , 'CommandBars' , ), 8448, (8448, (), [ (16397, 10, None, "IID('{55F88893-7708-11D1-ACEB-006008961DA5}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'CurrentItem' , 'CurrentItem' , ), 8450, (8450, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'EditorType' , 'EditorType' , ), 8464, (8464, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'ModifiedFormPages' , 'ModifiedFormPages' , ), 8454, (8454, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 8451, (8451, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 8452, (8452, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 64 , (3, 0, None, None) , 0 , )),
	(( 'HideFormPage' , 'PageName' , ), 8456, (8456, (), [ (8, 1, None, None) , ], 1 , 1 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'IsWordMail' , 'IsWordMail' , ), 8453, (8453, (), [ (16395, 10, None, None) , ], 1 , 1 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'SetCurrentFormPage' , 'PageName' , ), 8460, (8460, (), [ (8, 1, None, None) , ], 1 , 1 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'ShowFormPage' , 'PageName' , ), 8457, (8457, (), [ (8, 1, None, None) , ], 1 , 1 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'HTMLEditor' , 'HTMLEditor' , ), 8462, (8462, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'WordEditor' , 'WordEditor' , ), 8463, (8463, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'Caption' , 'Caption' , ), 8465, (8465, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'Height' , 'Height' , ), 8468, (8468, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'Height' , 'Height' , ), 8468, (8468, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'Left' , 'Left' , ), 8469, (8469, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Left' , 'Left' , ), 8469, (8469, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Top' , 'Top' , ), 8470, (8470, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'Top' , 'Top' , ), 8470, (8470, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'Width' , 'Width' , ), 8471, (8471, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 0 , )),
	(( 'Width' , 'Width' , ), 8471, (8471, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'WindowState' , 'WindowState' , ), 8466, (8466, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'WindowState' , 'WindowState' , ), 8466, (8466, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Activate' , ), 8467, (8467, (), [ ], 1 , 1 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
] _Inspectors_vtables_dispatch_ = 1 _Inspectors_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16397, 10, None, "IID('{00063058-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Add' , 'Item' , 'Inspector' , ), 95, (95, (), [ (9, 1, None, None) , 
			(16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
] _Items_vtables_dispatch_ = 1 _Items_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'RawTable' , 'RawTable' , ), 90, (90, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 64 , )),
	(( 'IncludeRecurrences' , 'IncludeRecurrences' , ), 206, (206, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'IncludeRecurrences' , 'IncludeRecurrences' , ), 206, (206, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Add' , 'Type' , 'Item' , ), 95, (95, (), [ (12, 17, None, None) , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 1 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Find' , 'Filter' , 'Item' , ), 98, (98, (), [ (8, 1, None, None) , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'FindNext' , 'Item' , ), 99, (99, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'GetFirst' , 'Item' , ), 86, (86, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'GetLast' , 'Item' , ), 88, (88, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'GetNext' , 'Item' , ), 87, (87, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'GetPrevious' , 'Item' , ), 89, (89, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'Remove' , 'Index' , ), 84, (84, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'ResetColumns' , ), 93, (93, (), [ ], 1 , 1 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'Restrict' , 'Filter' , 'Items' , ), 100, (100, (), [ (8, 1, None, None) , 
			(16393, 10, None, "IID('{00063041-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'SetColumns' , 'Columns' , ), 92, (92, (), [ (8, 1, None, None) , ], 1 , 1 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Sort' , 'Property' , 'Descending' , ), 97, (97, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 108 , (3, 0, None, None) , 0 , )),
] _JournalItem_vtables_dispatch_ = 1 _JournalItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'ContactNames' , 'ContactNames' , ), 3588, (3588, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
	(( 'ContactNames' , 'ContactNames' , ), 3588, (3588, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 228 , (3, 0, None, None) , 0 , )),
	(( 'DocPosted' , 'DocPosted' , ), 34577, (34577, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 232 , (3, 0, None, None) , 0 , )),
	(( 'DocPosted' , 'DocPosted' , ), 34577, (34577, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 236 , (3, 0, None, None) , 0 , )),
	(( 'DocPrinted' , 'DocPrinted' , ), 34574, (34574, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 240 , (3, 0, None, None) , 0 , )),
	(( 'DocPrinted' , 'DocPrinted' , ), 34574, (34574, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 244 , (3, 0, None, None) , 0 , )),
	(( 'DocRouted' , 'DocRouted' , ), 34576, (34576, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 248 , (3, 0, None, None) , 0 , )),
	(( 'DocRouted' , 'DocRouted' , ), 34576, (34576, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 252 , (3, 0, None, None) , 0 , )),
	(( 'DocSaved' , 'DocSaved' , ), 34575, (34575, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 256 , (3, 0, None, None) , 0 , )),
	(( 'DocSaved' , 'DocSaved' , ), 34575, (34575, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 260 , (3, 0, None, None) , 0 , )),
	(( 'Duration' , 'Duration' , ), 34567, (34567, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 264 , (3, 0, None, None) , 0 , )),
	(( 'Duration' , 'Duration' , ), 34567, (34567, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 268 , (3, 0, None, None) , 0 , )),
	(( 'End' , 'End' , ), 34568, (34568, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 272 , (3, 0, None, None) , 0 , )),
	(( 'End' , 'End' , ), 34568, (34568, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 276 , (3, 0, None, None) , 0 , )),
	(( 'Type' , 'Type' , ), 34560, (34560, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 280 , (3, 0, None, None) , 0 , )),
	(( 'Type' , 'Type' , ), 34560, (34560, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 284 , (3, 0, None, None) , 0 , )),
	(( 'Recipients' , 'Recipients' , ), 63508, (63508, (), [ (16393, 10, None, "IID('{0006303B-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 288 , (3, 0, None, None) , 0 , )),
	(( 'Start' , 'Start' , ), 34566, (34566, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 292 , (3, 0, None, None) , 0 , )),
	(( 'Start' , 'Start' , ), 34566, (34566, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 296 , (3, 0, None, None) , 0 , )),
	(( 'Forward' , 'Item' , ), 63507, (63507, (), [ (16397, 10, None, "IID('{00061033-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 300 , (3, 0, None, None) , 0 , )),
	(( 'Reply' , 'Item' , ), 63504, (63504, (), [ (16397, 10, None, "IID('{00061033-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 304 , (3, 0, None, None) , 0 , )),
	(( 'ReplyAll' , 'Item' , ), 63505, (63505, (), [ (16397, 10, None, "IID('{00061033-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 308 , (3, 0, None, None) , 0 , )),
	(( 'StartTimer' , ), 63269, (63269, (), [ ], 1 , 1 , 4 , 0 , 312 , (3, 0, None, None) , 0 , )),
	(( 'StopTimer' , ), 63270, (63270, (), [ ], 1 , 1 , 4 , 0 , 316 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 320 , (3, 0, None, None) , 0 , )),
] _MailItem_vtables_dispatch_ = 1 _MailItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'AlternateRecipientAllowed' , 'AlternateRecipientAllowed' , ), 2, (2, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
	(( 'AlternateRecipientAllowed' , 'AlternateRecipientAllowed' , ), 2, (2, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 228 , (3, 0, None, None) , 0 , )),
	(( 'AutoForwarded' , 'AutoForwarded' , ), 5, (5, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 232 , (3, 0, None, None) , 0 , )),
	(( 'AutoForwarded' , 'AutoForwarded' , ), 5, (5, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 236 , (3, 0, None, None) , 0 , )),
	(( 'BCC' , 'BCC' , ), 3586, (3586, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 240 , (3, 0, None, None) , 0 , )),
	(( 'BCC' , 'BCC' , ), 3586, (3586, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 244 , (3, 0, None, None) , 0 , )),
	(( 'CC' , 'CC' , ), 3587, (3587, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 248 , (3, 0, None, None) , 0 , )),
	(( 'CC' , 'CC' , ), 3587, (3587, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 252 , (3, 0, None, None) , 0 , )),
	(( 'DeferredDeliveryTime' , 'DeferredDeliveryTime' , ), 15, (15, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 256 , (3, 0, None, None) , 0 , )),
	(( 'DeferredDeliveryTime' , 'DeferredDeliveryTime' , ), 15, (15, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 260 , (3, 0, None, None) , 0 , )),
	(( 'DeleteAfterSubmit' , 'DeleteAfterSubmit' , ), 3585, (3585, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 264 , (3, 0, None, None) , 0 , )),
	(( 'DeleteAfterSubmit' , 'DeleteAfterSubmit' , ), 3585, (3585, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 268 , (3, 0, None, None) , 0 , )),
	(( 'ExpiryTime' , 'ExpiryTime' , ), 21, (21, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 272 , (3, 0, None, None) , 0 , )),
	(( 'ExpiryTime' , 'ExpiryTime' , ), 21, (21, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 276 , (3, 0, None, None) , 0 , )),
	(( 'FlagDueBy' , 'FlagDueBy' , ), 48, (48, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 280 , (3, 0, None, None) , 0 , )),
	(( 'FlagDueBy' , 'FlagDueBy' , ), 48, (48, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 284 , (3, 0, None, None) , 0 , )),
	(( 'FlagRequest' , 'FlagRequest' , ), 34096, (34096, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 288 , (3, 0, None, None) , 0 , )),
	(( 'FlagRequest' , 'FlagRequest' , ), 34096, (34096, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 292 , (3, 0, None, None) , 0 , )),
	(( 'FlagStatus' , 'FlagStatus' , ), 4240, (4240, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 296 , (3, 0, None, None) , 0 , )),
	(( 'FlagStatus' , 'FlagStatus' , ), 4240, (4240, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 300 , (3, 0, None, None) , 0 , )),
	(( 'HTMLBody' , 'HTMLBody' , ), 62468, (62468, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 304 , (3, 0, None, None) , 0 , )),
	(( 'HTMLBody' , 'HTMLBody' , ), 62468, (62468, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 308 , (3, 0, None, None) , 0 , )),
	(( 'OriginatorDeliveryReportRequested' , 'OriginatorDeliveryReportRequested' , ), 35, (35, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 312 , (3, 0, None, None) , 0 , )),
	(( 'OriginatorDeliveryReportRequested' , 'OriginatorDeliveryReportRequested' , ), 35, (35, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 316 , (3, 0, None, None) , 0 , )),
	(( 'ReadReceiptRequested' , 'ReadReceiptRequested' , ), 41, (41, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 320 , (3, 0, None, None) , 0 , )),
	(( 'ReadReceiptRequested' , 'ReadReceiptRequested' , ), 41, (41, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 324 , (3, 0, None, None) , 0 , )),
	(( 'ReceivedByEntryID' , 'ReceivedByEntryID' , ), 63, (63, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 328 , (3, 0, None, None) , 0 , )),
	(( 'ReceivedByName' , 'ReceivedByName' , ), 64, (64, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 332 , (3, 0, None, None) , 0 , )),
	(( 'ReceivedOnBehalfOfEntryID' , 'ReceivedOnBehalfOfEntryID' , ), 67, (67, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 336 , (3, 0, None, None) , 0 , )),
	(( 'ReceivedOnBehalfOfName' , 'ReceivedOnBehalfOfName' , ), 68, (68, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 340 , (3, 0, None, None) , 0 , )),
	(( 'ReceivedTime' , 'ReceivedTime' , ), 3590, (3590, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 344 , (3, 0, None, None) , 0 , )),
	(( 'RecipientReassignmentProhibited' , 'RecipientReassignmentProhibited' , ), 43, (43, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 348 , (3, 0, None, None) , 0 , )),
	(( 'RecipientReassignmentProhibited' , 'RecipientReassignmentProhibited' , ), 43, (43, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 352 , (3, 0, None, None) , 0 , )),
	(( 'Recipients' , 'Recipients' , ), 63508, (63508, (), [ (16393, 10, None, "IID('{0006303B-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 356 , (3, 0, None, None) , 0 , )),
	(( 'ReminderOverrideDefault' , 'ReminderOverrideDefault' , ), 34076, (34076, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 360 , (3, 0, None, None) , 0 , )),
	(( 'ReminderOverrideDefault' , 'ReminderOverrideDefault' , ), 34076, (34076, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 364 , (3, 0, None, None) , 0 , )),
	(( 'ReminderPlaySound' , 'ReminderPlaySound' , ), 34078, (34078, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 368 , (3, 0, None, None) , 0 , )),
	(( 'ReminderPlaySound' , 'ReminderPlaySound' , ), 34078, (34078, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 372 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSet' , 'ReminderSet' , ), 34051, (34051, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 376 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSet' , 'ReminderSet' , ), 34051, (34051, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 380 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSoundFile' , 'ReminderSoundFile' , ), 34079, (34079, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 384 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSoundFile' , 'ReminderSoundFile' , ), 34079, (34079, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 388 , (3, 0, None, None) , 0 , )),
	(( 'ReminderTime' , 'ReminderTime' , ), 34050, (34050, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 392 , (3, 0, None, None) , 0 , )),
	(( 'ReminderTime' , 'ReminderTime' , ), 34050, (34050, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 396 , (3, 0, None, None) , 0 , )),
	(( 'RemoteStatus' , 'RemoteStatus' , ), 34065, (34065, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 400 , (3, 0, None, None) , 0 , )),
	(( 'RemoteStatus' , 'RemoteStatus' , ), 34065, (34065, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 404 , (3, 0, None, None) , 0 , )),
	(( 'ReplyRecipientNames' , 'ReplyRecipientNames' , ), 80, (80, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 408 , (3, 0, None, None) , 0 , )),
	(( 'ReplyRecipients' , 'ReplyRecipients' , ), 61459, (61459, (), [ (16393, 10, None, "IID('{0006303B-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 412 , (3, 0, None, None) , 0 , )),
	(( 'SaveSentMessageFolder' , 'SaveSentMessageFolder' , ), 62465, (62465, (), [ (16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 416 , (3, 0, None, None) , 0 , )),
	(( 'SaveSentMessageFolder' , 'SaveSentMessageFolder' , ), 62465, (62465, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 8 , 4 , 0 , 420 , (3, 0, None, None) , 0 , )),
	(( 'SenderName' , 'SenderName' , ), 3098, (3098, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 424 , (3, 0, None, None) , 0 , )),
	(( 'Sent' , 'Sent' , ), 62466, (62466, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 428 , (3, 0, None, None) , 0 , )),
	(( 'SentOn' , 'SentOn' , ), 57, (57, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 432 , (3, 0, None, None) , 0 , )),
	(( 'SentOnBehalfOfName' , 'SentOnBehalfOfName' , ), 66, (66, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 436 , (3, 0, None, None) , 0 , )),
	(( 'SentOnBehalfOfName' , 'SentOnBehalfOfName' , ), 66, (66, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 440 , (3, 0, None, None) , 0 , )),
	(( 'Submitted' , 'Submitted' , ), 62467, (62467, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 444 , (3, 0, None, None) , 0 , )),
	(( 'To' , 'To' , ), 3588, (3588, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 448 , (3, 0, None, None) , 0 , )),
	(( 'To' , 'To' , ), 3588, (3588, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 452 , (3, 0, None, None) , 0 , )),
	(( 'VotingOptions' , 'VotingOptions' , ), 61467, (61467, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 456 , (3, 0, None, None) , 0 , )),
	(( 'VotingOptions' , 'VotingOptions' , ), 61467, (61467, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 460 , (3, 0, None, None) , 0 , )),
	(( 'VotingResponse' , 'VotingResponse' , ), 34084, (34084, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 464 , (3, 0, None, None) , 0 , )),
	(( 'VotingResponse' , 'VotingResponse' , ), 34084, (34084, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 468 , (3, 0, None, None) , 0 , )),
	(( 'ClearConversationIndex' , ), 63522, (63522, (), [ ], 1 , 1 , 4 , 0 , 472 , (3, 0, None, None) , 0 , )),
	(( 'Forward' , 'Item' , ), 63507, (63507, (), [ (16397, 10, None, "IID('{00061033-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 476 , (3, 0, None, None) , 0 , )),
	(( 'Reply' , 'Item' , ), 63504, (63504, (), [ (16397, 10, None, "IID('{00061033-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 480 , (3, 0, None, None) , 0 , )),
	(( 'ReplyAll' , 'Item' , ), 63505, (63505, (), [ (16397, 10, None, "IID('{00061033-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 484 , (3, 0, None, None) , 0 , )),
	(( 'Send' , ), 61557, (61557, (), [ ], 1 , 1 , 4 , 0 , 488 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 492 , (3, 0, None, None) , 0 , )),
] _MeetingItem_vtables_dispatch_ = 1 _MeetingItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'AutoForwarded' , 'AutoForwarded' , ), 5, (5, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
	(( 'AutoForwarded' , 'AutoForwarded' , ), 5, (5, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 228 , (3, 0, None, None) , 0 , )),
	(( 'DeferredDeliveryTime' , 'DeferredDeliveryTime' , ), 15, (15, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 232 , (3, 0, None, None) , 0 , )),
	(( 'DeferredDeliveryTime' , 'DeferredDeliveryTime' , ), 15, (15, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 236 , (3, 0, None, None) , 0 , )),
	(( 'DeleteAfterSubmit' , 'DeleteAfterSubmit' , ), 3585, (3585, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 240 , (3, 0, None, None) , 0 , )),
	(( 'DeleteAfterSubmit' , 'DeleteAfterSubmit' , ), 3585, (3585, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 244 , (3, 0, None, None) , 0 , )),
	(( 'ExpiryTime' , 'ExpiryTime' , ), 21, (21, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 248 , (3, 0, None, None) , 0 , )),
	(( 'ExpiryTime' , 'ExpiryTime' , ), 21, (21, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 252 , (3, 0, None, None) , 0 , )),
	(( 'FlagDueBy' , 'FlagDueBy' , ), 48, (48, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 256 , (3, 0, None, None) , 0 , )),
	(( 'FlagDueBy' , 'FlagDueBy' , ), 48, (48, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 260 , (3, 0, None, None) , 0 , )),
	(( 'FlagRequest' , 'FlagRequest' , ), 34096, (34096, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 264 , (3, 0, None, None) , 0 , )),
	(( 'FlagRequest' , 'FlagRequest' , ), 34096, (34096, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 268 , (3, 0, None, None) , 0 , )),
	(( 'FlagStatus' , 'FlagStatus' , ), 4240, (4240, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 272 , (3, 0, None, None) , 0 , )),
	(( 'FlagStatus' , 'FlagStatus' , ), 4240, (4240, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 276 , (3, 0, None, None) , 0 , )),
	(( 'OriginatorDeliveryReportRequested' , 'OriginatorDeliveryReportRequested' , ), 35, (35, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 280 , (3, 0, None, None) , 0 , )),
	(( 'OriginatorDeliveryReportRequested' , 'OriginatorDeliveryReportRequested' , ), 35, (35, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 284 , (3, 0, None, None) , 0 , )),
	(( 'ReceivedTime' , 'ReceivedTime' , ), 3590, (3590, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 288 , (3, 0, None, None) , 0 , )),
	(( 'ReceivedTime' , 'ReceivedTime' , ), 3590, (3590, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 292 , (3, 0, None, None) , 0 , )),
	(( 'Recipients' , 'Recipients' , ), 63508, (63508, (), [ (16393, 10, None, "IID('{0006303B-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 296 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSet' , 'ReminderSet' , ), 34051, (34051, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 300 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSet' , 'ReminderSet' , ), 34051, (34051, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 304 , (3, 0, None, None) , 0 , )),
	(( 'ReminderTime' , 'ReminderTime' , ), 34050, (34050, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 308 , (3, 0, None, None) , 0 , )),
	(( 'ReminderTime' , 'ReminderTime' , ), 34050, (34050, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 312 , (3, 0, None, None) , 0 , )),
	(( 'ReplyRecipients' , 'ReplyRecipients' , ), 61459, (61459, (), [ (16393, 10, None, "IID('{0006303B-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 316 , (3, 0, None, None) , 0 , )),
	(( 'SaveSentMessageFolder' , 'SaveSentMessageFolder' , ), 62465, (62465, (), [ (16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 320 , (3, 0, None, None) , 0 , )),
	(( 'SaveSentMessageFolder' , 'SaveSentMessageFolder' , ), 62465, (62465, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 8 , 4 , 0 , 324 , (3, 0, None, None) , 0 , )),
	(( 'SenderName' , 'SenderName' , ), 3098, (3098, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 328 , (3, 0, None, None) , 0 , )),
	(( 'Sent' , 'Sent' , ), 62466, (62466, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 332 , (3, 0, None, None) , 0 , )),
	(( 'SentOn' , 'SentOn' , ), 57, (57, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 336 , (3, 0, None, None) , 0 , )),
	(( 'Submitted' , 'Submitted' , ), 62467, (62467, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 340 , (3, 0, None, None) , 0 , )),
	(( 'Forward' , 'Item' , ), 63507, (63507, (), [ (16397, 10, None, "IID('{00061036-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 344 , (3, 0, None, None) , 0 , )),
	(( 'GetAssociatedAppointment' , 'AddToCalendar' , 'Item' , ), 63328, (63328, (), [ (11, 1, None, None) , 
			(16397, 10, None, "IID('{00061030-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 348 , (3, 0, None, None) , 0 , )),
	(( 'Reply' , 'Item' , ), 63504, (63504, (), [ (16397, 10, None, "IID('{00061033-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 352 , (3, 0, None, None) , 0 , )),
	(( 'ReplyAll' , 'Item' , ), 63505, (63505, (), [ (16397, 10, None, "IID('{00061033-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 356 , (3, 0, None, None) , 0 , )),
	(( 'Send' , ), 61557, (61557, (), [ ], 1 , 1 , 4 , 0 , 360 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 364 , (3, 0, None, None) , 0 , )),
] _NameSpace_vtables_dispatch_ = 1 _NameSpace_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'CurrentUser' , 'CurrentUser' , ), 8449, (8449, (), [ (16393, 10, None, "IID('{00063045-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Folders' , 'Folders' , ), 8451, (8451, (), [ (16393, 10, None, "IID('{00063040-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Type' , 'Type' , ), 8452, (8452, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'AddressLists' , 'AddressLists' , ), 8461, (8461, (), [ (16393, 10, None, "IID('{00063048-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'CreateRecipient' , 'RecipientName' , 'Recipient' , ), 8458, (8458, (), [ (8, 1, None, None) , 
			(16393, 10, None, "IID('{00063045-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'GetDefaultFolder' , 'FolderType' , 'Folder' , ), 8459, (8459, (), [ (3, 1, None, None) , 
			(16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'GetFolderFromID' , 'EntryIDFolder' , 'EntryIDStore' , 'Folder' , ), 8456, (8456, (), [ 
			(8, 1, None, None) , (12, 17, None, None) , (16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 1 , 68 , (3, 0, None, None) , 0 , )),
	(( 'GetItemFromID' , 'EntryIDItem' , 'EntryIDStore' , 'Item' , ), 8457, (8457, (), [ 
			(8, 1, None, None) , (12, 17, None, None) , (16393, 10, None, None) , ], 1 , 1 , 4 , 1 , 72 , (3, 0, None, None) , 0 , )),
	(( 'GetRecipientFromID' , 'EntryID' , 'Recipient' , ), 8455, (8455, (), [ (8, 1, None, None) , 
			(16393, 10, None, "IID('{00063045-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'GetSharedDefaultFolder' , 'Recipient' , 'FolderType' , 'Folder' , ), 8460, (8460, (), [ 
			(9, 1, None, "IID('{00063045-0000-0000-C000-000000000046}')") , (3, 1, None, None) , (16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'Logoff' , ), 8454, (8454, (), [ ], 1 , 1 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'Logon' , 'Profile' , 'Password' , 'ShowDialog' , 'NewSession' , 
			), 8453, (8453, (), [ (12, 17, None, None) , (12, 17, None, None) , (12, 17, None, None) , (12, 17, None, None) , ], 1 , 1 , 4 , 4 , 88 , (3, 0, None, None) , 0 , )),
	(( 'PickFolder' , 'Folder' , ), 8462, (8462, (), [ (16393, 10, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'RefreshRemoteHeaders' , ), 8471, (8471, (), [ ], 1 , 1 , 4 , 0 , 96 , (3, 0, None, None) , 64 , )),
	(( 'SyncObjects' , 'SyncObjects' , ), 8472, (8472, (), [ (16393, 10, None, "IID('{00063086-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'AddStore' , 'Store' , ), 8473, (8473, (), [ (12, 1, None, None) , ], 1 , 1 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'RemoveStore' , 'Folder' , ), 8474, (8474, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 108 , (3, 0, None, None) , 64 , )),
] _NoteItem_vtables_dispatch_ = 1 _NoteItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Color' , 'Color' , ), 35584, (35584, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Color' , 'Color' , ), 35584, (35584, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Height' , 'Height' , ), 35587, (35587, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'Height' , 'Height' , ), 35587, (35587, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'Left' , 'Left' , ), 35588, (35588, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'Left' , 'Left' , ), 35588, (35588, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 63392, (63392, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'Top' , 'Top' , ), 35589, (35589, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 0 , )),
	(( 'Top' , 'Top' , ), 35589, (35589, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'Width' , 'Width' , ), 35586, (35586, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Width' , 'Width' , ), 35586, (35586, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 148 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
] _OutlookBarGroups_vtables_dispatch_ = 1 _OutlookBarGroups_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, "IID('{00063073-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Add' , 'Name' , 'Index' , 'Group' , ), 95, (95, (), [ 
			(8, 1, None, None) , (12, 17, None, None) , (16393, 10, None, "IID('{00063073-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 1 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Remove' , 'Index' , ), 84, (84, (), [ (12, 1, None, None) , ], 1 , 1 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
] _OutlookBarPane_vtables_dispatch_ = 1 _OutlookBarPane_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Contents' , 'Contents' , ), 8448, (8448, (), [ (16393, 10, None, "IID('{00063071-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'CurrentGroup' , 'CurrentGroup' , ), 8449, (8449, (), [ (16393, 10, None, "IID('{00063073-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'CurrentGroup' , 'CurrentGroup' , ), 8449, (8449, (), [ (9, 1, None, "IID('{00063073-0000-0000-C000-000000000046}')") , ], 1 , 8 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 0, (0, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Visible' , 'Visible' , ), 8451, (8451, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Visible' , 'Visible' , ), 8451, (8451, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
] _OutlookBarShortcuts_vtables_dispatch_ = 1 _OutlookBarShortcuts_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Count' , 'Count' , ), 80, (80, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Item' , 'Index' , 'Item' , ), 81, (81, (), [ (12, 1, None, None) , 
			(16393, 10, None, "IID('{00063075-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Add' , 'Target' , 'Name' , 'Index' , 'Shortcut' , 
			), 95, (95, (), [ (12, 1, None, None) , (8, 1, None, None) , (12, 17, None, None) , (16393, 10, None, "IID('{00063075-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 1 , 52 , (3, 0, None, None) , 0 , )),
	(( 'Remove' , 'Index' , ), 84, (84, (), [ (12, 1, None, None) , ], 1 , 1 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
] _PostItem_vtables_dispatch_ = 1 _PostItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'ExpiryTime' , 'ExpiryTime' , ), 21, (21, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
	(( 'ExpiryTime' , 'ExpiryTime' , ), 21, (21, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 228 , (3, 0, None, None) , 0 , )),
	(( 'HTMLBody' , 'HTMLBody' , ), 62468, (62468, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 232 , (3, 0, None, None) , 0 , )),
	(( 'HTMLBody' , 'HTMLBody' , ), 62468, (62468, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 236 , (3, 0, None, None) , 0 , )),
	(( 'ReceivedTime' , 'ReceivedTime' , ), 3590, (3590, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 240 , (3, 0, None, None) , 0 , )),
	(( 'SenderName' , 'SenderName' , ), 3098, (3098, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 244 , (3, 0, None, None) , 0 , )),
	(( 'SentOn' , 'SentOn' , ), 57, (57, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 248 , (3, 0, None, None) , 0 , )),
	(( 'ClearConversationIndex' , ), 63522, (63522, (), [ ], 1 , 1 , 4 , 0 , 252 , (3, 0, None, None) , 0 , )),
	(( 'Forward' , 'Item' , ), 63507, (63507, (), [ (16397, 10, None, "IID('{00061033-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 256 , (3, 0, None, None) , 0 , )),
	(( 'Post' , ), 61557, (61557, (), [ ], 1 , 1 , 4 , 0 , 260 , (3, 0, None, None) , 0 , )),
	(( 'Reply' , 'Item' , ), 63504, (63504, (), [ (16397, 10, None, "IID('{00061033-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 264 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 268 , (3, 0, None, None) , 0 , )),
] _RemoteItem_vtables_dispatch_ = 1 _RemoteItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'HasAttachment' , 'HasAttachment' , ), 36615, (36615, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
	(( 'RemoteMessageClass' , 'RemoteMessageClass' , ), 36610, (36610, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 228 , (3, 0, None, None) , 0 , )),
	(( 'TransferSize' , 'TransferSize' , ), 36613, (36613, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 232 , (3, 0, None, None) , 0 , )),
	(( 'TransferTime' , 'TransferTime' , ), 36612, (36612, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 236 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 240 , (3, 0, None, None) , 0 , )),
] _ReportItem_vtables_dispatch_ = 1 _ReportItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
] _SyncObject_vtables_dispatch_ = 1 _SyncObject_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Name' , 'Name' , ), 8448, (8448, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Start' , ), 8449, (8449, (), [ ], 1 , 1 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'Stop' , ), 8450, (8450, (), [ ], 1 , 1 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
] _TaskItem_vtables_dispatch_ = 1 _TaskItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'ActualWork' , 'ActualWork' , ), 33040, (33040, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
	(( 'ActualWork' , 'ActualWork' , ), 33040, (33040, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 228 , (3, 0, None, None) , 0 , )),
	(( 'CardData' , 'CardData' , ), 33067, (33067, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 232 , (3, 0, None, None) , 0 , )),
	(( 'CardData' , 'CardData' , ), 33067, (33067, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 236 , (3, 0, None, None) , 0 , )),
	(( 'Complete' , 'Complete' , ), 33052, (33052, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 240 , (3, 0, None, None) , 0 , )),
	(( 'Complete' , 'Complete' , ), 33052, (33052, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 244 , (3, 0, None, None) , 0 , )),
	(( 'Contacts' , 'Contacts' , ), 34106, (34106, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 248 , (3, 0, None, None) , 0 , )),
	(( 'Contacts' , 'Contacts' , ), 34106, (34106, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 252 , (3, 0, None, None) , 0 , )),
	(( 'ContactNames' , 'ContactNames' , ), 34108, (34108, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 256 , (3, 0, None, None) , 0 , )),
	(( 'ContactNames' , 'ContactNames' , ), 34108, (34108, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 260 , (3, 0, None, None) , 0 , )),
	(( 'DateCompleted' , 'DateCompleted' , ), 33039, (33039, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 264 , (3, 0, None, None) , 0 , )),
	(( 'DateCompleted' , 'DateCompleted' , ), 33039, (33039, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 268 , (3, 0, None, None) , 0 , )),
	(( 'DelegationState' , 'DelegationState' , ), 33066, (33066, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 272 , (3, 0, None, None) , 0 , )),
	(( 'Delegator' , 'Delegator' , ), 33057, (33057, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 276 , (3, 0, None, None) , 0 , )),
	(( 'DueDate' , 'DueDate' , ), 33029, (33029, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 280 , (3, 0, None, None) , 0 , )),
	(( 'DueDate' , 'DueDate' , ), 33029, (33029, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 284 , (3, 0, None, None) , 0 , )),
	(( 'IsRecurring' , 'IsRecurring' , ), 62999, (62999, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 288 , (3, 0, None, None) , 0 , )),
	(( 'Ordinal' , 'Ordinal' , ), 33059, (33059, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 292 , (3, 0, None, None) , 0 , )),
	(( 'Ordinal' , 'Ordinal' , ), 33059, (33059, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 296 , (3, 0, None, None) , 0 , )),
	(( 'Owner' , 'Owner' , ), 33055, (33055, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 300 , (3, 0, None, None) , 0 , )),
	(( 'Owner' , 'Owner' , ), 33055, (33055, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 304 , (3, 0, None, None) , 0 , )),
	(( 'Ownership' , 'Ownership' , ), 33065, (33065, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 308 , (3, 0, None, None) , 0 , )),
	(( 'PercentComplete' , 'PercentComplete' , ), 63007, (63007, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 312 , (3, 0, None, None) , 0 , )),
	(( 'PercentComplete' , 'PercentComplete' , ), 63007, (63007, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 316 , (3, 0, None, None) , 0 , )),
	(( 'Recipients' , 'Recipients' , ), 63508, (63508, (), [ (16393, 10, None, "IID('{0006303B-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 320 , (3, 0, None, None) , 0 , )),
	(( 'ReminderTime' , 'ReminderTime' , ), 34050, (34050, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 324 , (3, 0, None, None) , 0 , )),
	(( 'ReminderTime' , 'ReminderTime' , ), 34050, (34050, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 328 , (3, 0, None, None) , 0 , )),
	(( 'ReminderOverrideDefault' , 'ReminderOverrideDefault' , ), 34076, (34076, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 332 , (3, 0, None, None) , 0 , )),
	(( 'ReminderOverrideDefault' , 'ReminderOverrideDefault' , ), 34076, (34076, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 336 , (3, 0, None, None) , 0 , )),
	(( 'ReminderPlaySound' , 'ReminderPlaySound' , ), 34078, (34078, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 340 , (3, 0, None, None) , 0 , )),
	(( 'ReminderPlaySound' , 'ReminderPlaySound' , ), 34078, (34078, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 344 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSet' , 'ReminderSet' , ), 34051, (34051, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 348 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSet' , 'ReminderSet' , ), 34051, (34051, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 352 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSoundFile' , 'ReminderSoundFile' , ), 34079, (34079, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 356 , (3, 0, None, None) , 0 , )),
	(( 'ReminderSoundFile' , 'ReminderSoundFile' , ), 34079, (34079, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 360 , (3, 0, None, None) , 0 , )),
	(( 'ResponseState' , 'ResponseState' , ), 63011, (63011, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 364 , (3, 0, None, None) , 0 , )),
	(( 'Role' , 'Role' , ), 33063, (33063, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 368 , (3, 0, None, None) , 0 , )),
	(( 'Role' , 'Role' , ), 33063, (33063, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 372 , (3, 0, None, None) , 0 , )),
	(( 'SchedulePlusPriority' , 'SchedulePlusPriority' , ), 33071, (33071, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 376 , (3, 0, None, None) , 0 , )),
	(( 'SchedulePlusPriority' , 'SchedulePlusPriority' , ), 33071, (33071, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 380 , (3, 0, None, None) , 0 , )),
	(( 'StartDate' , 'StartDate' , ), 33028, (33028, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 384 , (3, 0, None, None) , 0 , )),
	(( 'StartDate' , 'StartDate' , ), 33028, (33028, (), [ (7, 1, None, None) , ], 1 , 4 , 4 , 0 , 388 , (3, 0, None, None) , 0 , )),
	(( 'Status' , 'Status' , ), 33025, (33025, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 392 , (3, 0, None, None) , 0 , )),
	(( 'Status' , 'Status' , ), 33025, (33025, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 396 , (3, 0, None, None) , 0 , )),
	(( 'StatusOnCompletionRecipients' , 'StatusOnCompletionRecipients' , ), 3586, (3586, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 400 , (3, 0, None, None) , 0 , )),
	(( 'StatusOnCompletionRecipients' , 'StatusOnCompletionRecipients' , ), 3586, (3586, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 404 , (3, 0, None, None) , 0 , )),
	(( 'StatusUpdateRecipients' , 'StatusUpdateRecipients' , ), 3587, (3587, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 408 , (3, 0, None, None) , 0 , )),
	(( 'StatusUpdateRecipients' , 'StatusUpdateRecipients' , ), 3587, (3587, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 412 , (3, 0, None, None) , 0 , )),
	(( 'TeamTask' , 'TeamTask' , ), 33027, (33027, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 416 , (3, 0, None, None) , 0 , )),
	(( 'TeamTask' , 'TeamTask' , ), 33027, (33027, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 420 , (3, 0, None, None) , 0 , )),
	(( 'TotalWork' , 'TotalWork' , ), 33041, (33041, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 424 , (3, 0, None, None) , 0 , )),
	(( 'TotalWork' , 'TotalWork' , ), 33041, (33041, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 428 , (3, 0, None, None) , 0 , )),
	(( 'Assign' , 'Item' , ), 63008, (63008, (), [ (16397, 10, None, "IID('{00061032-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 432 , (3, 0, None, None) , 0 , )),
	(( 'CancelResponseState' , ), 63010, (63010, (), [ ], 1 , 1 , 4 , 0 , 436 , (3, 0, None, None) , 0 , )),
	(( 'ClearRecurrencePattern' , ), 61605, (61605, (), [ ], 1 , 1 , 4 , 0 , 440 , (3, 0, None, None) , 0 , )),
	(( 'GetRecurrencePattern' , 'RecurrencPattern' , ), 61604, (61604, (), [ (16393, 10, None, "IID('{00063044-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 444 , (3, 0, None, None) , 0 , )),
	(( 'MarkComplete' , ), 62989, (62989, (), [ ], 1 , 1 , 4 , 0 , 448 , (3, 0, None, None) , 0 , )),
	(( 'Respond' , 'Response' , 'fNoUI' , 'fAdditionalTextDialog' , 'Item' , 
			), 63009, (63009, (), [ (3, 1, None, None) , (12, 1, None, None) , (12, 1, None, None) , (16397, 10, None, "IID('{00061032-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 452 , (3, 0, None, None) , 0 , )),
	(( 'Send' , ), 61557, (61557, (), [ ], 1 , 1 , 4 , 0 , 456 , (3, 0, None, None) , 0 , )),
	(( 'SkipRecurrence' , 'flg' , ), 63012, (63012, (), [ (16395, 10, None, None) , ], 1 , 1 , 4 , 0 , 460 , (3, 0, None, None) , 0 , )),
	(( 'StatusReport' , 'StatusReport' , ), 62994, (62994, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 464 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 468 , (3, 0, None, None) , 0 , )),
] _TaskRequestAcceptItem_vtables_dispatch_ = 1 _TaskRequestAcceptItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'GetAssociatedTask' , 'AddToTaskList' , 'Item' , ), 61460, (61460, (), [ (11, 1, None, None) , 
			(16397, 10, None, "IID('{00061032-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 228 , (3, 0, None, None) , 0 , )),
] _TaskRequestDeclineItem_vtables_dispatch_ = 1 _TaskRequestDeclineItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'GetAssociatedTask' , 'AddToTaskList' , 'Item' , ), 61460, (61460, (), [ (11, 1, None, None) , 
			(16397, 10, None, "IID('{00061032-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 228 , (3, 0, None, None) , 0 , )),
] _TaskRequestItem_vtables_dispatch_ = 1 _TaskRequestItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'GetAssociatedTask' , 'AddToTaskList' , 'Item' , ), 61460, (61460, (), [ (11, 1, None, None) , 
			(16397, 10, None, "IID('{00061032-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 228 , (3, 0, None, None) , 0 , )),
] _TaskRequestUpdateItem_vtables_dispatch_ = 1 _TaskRequestUpdateItem_vtables_ = [
	(( 'Application' , 'Application' , ), 61440, (61440, (), [ (16393, 10, None, "IID('{00063001-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'Class' , 'Class' , ), 61450, (61450, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'Session' , 'Session' , ), 61451, (61451, (), [ (16393, 10, None, "IID('{00063002-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'Parent' , 'Parent' , ), 61441, (61441, (), [ (16393, 10, None, None) , ], 1 , 2 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'Actions' , 'Actions' , ), 63511, (63511, (), [ (16393, 10, None, "IID('{0006303E-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
	(( 'Attachments' , 'Attachments' , ), 63509, (63509, (), [ (16393, 10, None, "IID('{0006303C-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 48 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 52 , (3, 0, None, None) , 0 , )),
	(( 'BillingInformation' , 'BillingInformation' , ), 34101, (34101, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 56 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 60 , (3, 0, None, None) , 0 , )),
	(( 'Body' , 'Body' , ), 37120, (37120, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 64 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 68 , (3, 0, None, None) , 0 , )),
	(( 'Categories' , 'Categories' , ), 36865, (36865, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 72 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 76 , (3, 0, None, None) , 0 , )),
	(( 'Companies' , 'Companies' , ), 34107, (34107, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 80 , (3, 0, None, None) , 0 , )),
	(( 'ConversationIndex' , 'ConversationIndex' , ), 113, (113, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 84 , (3, 0, None, None) , 0 , )),
	(( 'ConversationTopic' , 'ConversationTopic' , ), 112, (112, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 88 , (3, 0, None, None) , 0 , )),
	(( 'CreationTime' , 'CreationTime' , ), 12295, (12295, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 92 , (3, 0, None, None) , 0 , )),
	(( 'EntryID' , 'EntryID' , ), 61470, (61470, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 96 , (3, 0, None, None) , 0 , )),
	(( 'FormDescription' , 'FormDescription' , ), 61589, (61589, (), [ (16393, 10, None, "IID('{00063046-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 100 , (3, 0, None, None) , 0 , )),
	(( 'GetInspector' , 'GetInspector' , ), 61502, (61502, (), [ (16393, 10, None, "IID('{00063005-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 104 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 108 , (3, 0, None, None) , 0 , )),
	(( 'Importance' , 'Importance' , ), 23, (23, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 112 , (3, 0, None, None) , 0 , )),
	(( 'LastModificationTime' , 'LastModificationTime' , ), 12296, (12296, (), [ (16391, 10, None, None) , ], 1 , 2 , 4 , 0 , 116 , (3, 0, None, None) , 0 , )),
	(( 'MAPIOBJECT' , 'MAPIOBJECT' , ), 61696, (61696, (), [ (16397, 10, None, None) , ], 1 , 2 , 4 , 0 , 120 , (3, 0, None, None) , 64 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 124 , (3, 0, None, None) , 0 , )),
	(( 'MessageClass' , 'MessageClass' , ), 26, (26, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 128 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 132 , (3, 0, None, None) , 0 , )),
	(( 'Mileage' , 'Mileage' , ), 34100, (34100, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 136 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 140 , (3, 0, None, None) , 0 , )),
	(( 'NoAging' , 'NoAging' , ), 34062, (34062, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 144 , (3, 0, None, None) , 0 , )),
	(( 'OutlookInternalVersion' , 'OutlookInternalVersion' , ), 34130, (34130, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 148 , (3, 0, None, None) , 0 , )),
	(( 'OutlookVersion' , 'OutlookVersion' , ), 34132, (34132, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 152 , (3, 0, None, None) , 0 , )),
	(( 'Saved' , 'Saved' , ), 61603, (61603, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 156 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 160 , (3, 0, None, None) , 0 , )),
	(( 'Sensitivity' , 'Sensitivity' , ), 54, (54, (), [ (3, 1, None, None) , ], 1 , 4 , 4 , 0 , 164 , (3, 0, None, None) , 0 , )),
	(( 'Size' , 'Size' , ), 3592, (3592, (), [ (16387, 10, None, None) , ], 1 , 2 , 4 , 0 , 168 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (16392, 10, None, None) , ], 1 , 2 , 4 , 0 , 172 , (3, 0, None, None) , 0 , )),
	(( 'Subject' , 'Subject' , ), 55, (55, (), [ (8, 1, None, None) , ], 1 , 4 , 4 , 0 , 176 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (16395, 10, None, None) , ], 1 , 2 , 4 , 0 , 180 , (3, 0, None, None) , 0 , )),
	(( 'UnRead' , 'UnRead' , ), 61468, (61468, (), [ (11, 1, None, None) , ], 1 , 4 , 4 , 0 , 184 , (3, 0, None, None) , 0 , )),
	(( 'UserProperties' , 'UserProperties' , ), 63510, (63510, (), [ (16393, 10, None, "IID('{0006303D-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 188 , (3, 0, None, None) , 0 , )),
	(( 'Close' , 'SaveMode' , ), 61475, (61475, (), [ (3, 1, None, None) , ], 1 , 1 , 4 , 0 , 192 , (3, 0, None, None) , 0 , )),
	(( 'Copy' , 'Item' , ), 61490, (61490, (), [ (16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 196 , (3, 0, None, None) , 0 , )),
	(( 'Delete' , ), 61514, (61514, (), [ ], 1 , 1 , 4 , 0 , 200 , (3, 0, None, None) , 0 , )),
	(( 'Display' , 'Modal' , ), 61606, (61606, (), [ (12, 17, None, None) , ], 1 , 1 , 4 , 1 , 204 , (3, 0, None, None) , 0 , )),
	(( 'Move' , 'DestFldr' , 'Item' , ), 61492, (61492, (), [ (9, 1, None, "IID('{00063006-0000-0000-C000-000000000046}')") , 
			(16393, 10, None, None) , ], 1 , 1 , 4 , 0 , 208 , (3, 0, None, None) , 0 , )),
	(( 'PrintOut' , ), 61491, (61491, (), [ ], 1 , 1 , 4 , 0 , 212 , (3, 0, None, None) , 0 , )),
	(( 'Save' , ), 61512, (61512, (), [ ], 1 , 1 , 4 , 0 , 216 , (3, 0, None, None) , 0 , )),
	(( 'SaveAs' , 'Path' , 'Type' , ), 61521, (61521, (), [ (8, 1, None, None) , 
			(12, 17, None, None) , ], 1 , 1 , 4 , 1 , 220 , (3, 0, None, None) , 0 , )),
	(( 'GetAssociatedTask' , 'AddToTaskList' , 'Item' , ), 61460, (61460, (), [ (11, 1, None, None) , 
			(16397, 10, None, "IID('{00061032-0000-0000-C000-000000000046}')") , ], 1 , 1 , 4 , 0 , 224 , (3, 0, None, None) , 0 , )),
	(( 'Links' , 'Links' , ), 62469, (62469, (), [ (16393, 10, None, "IID('{0006308A-0000-0000-C000-000000000046}')") , ], 1 , 2 , 4 , 0 , 228 , (3, 0, None, None) , 0 , )),
] RecordMap = {
} CLSIDToClassMap = {
	'{50BB9B50-811D-11CE-B565-00AA00608FAA}' : _DDocSiteControlEvents,
	'{0006F023-0000-0000-C000-000000000046}' : _RecipientControl,
	'{00061050-0000-0000-C000-000000000046}' : TaskRequestItem,
	'{00061051-0000-0000-C000-000000000046}' : TaskRequestUpdateItem,
	'{00061052-0000-0000-C000-000000000046}' : TaskRequestAcceptItem,
	'{00061053-0000-0000-C000-000000000046}' : TaskRequestDeclineItem,
	'{43507DD0-811D-11CE-B565-00AA00608FAA}' : _IDocSiteControl,
	'{00063001-0000-0000-C000-000000000046}' : _Application,
	'{00063002-0000-0000-C000-000000000046}' : _NameSpace,
	'{00063003-0000-0000-C000-000000000046}' : _Explorer,
	'{00063005-0000-0000-C000-000000000046}' : _Inspector,
	'{00063006-0000-0000-C000-000000000046}' : MAPIFolder,
	'{00063007-0000-0000-C000-000000000046}' : Attachment,
	'{00063008-0000-0000-C000-000000000046}' : _Inspectors,
	'{00063009-0000-0000-C000-000000000046}' : Panes,
	'{0006300A-0000-0000-C000-000000000046}' : _Explorers,
	'{00063020-0000-0000-C000-000000000046}' : _DocumentItem,
	'{00063021-0000-0000-C000-000000000046}' : _ContactItem,
	'{00063022-0000-0000-C000-000000000046}' : _JournalItem,
	'{00063023-0000-0000-C000-000000000046}' : _RemoteItem,
	'{00063024-0000-0000-C000-000000000046}' : _PostItem,
	'{00063025-0000-0000-C000-000000000046}' : _NoteItem,
	'{00063026-0000-0000-C000-000000000046}' : _ReportItem,
	'{00061030-0000-0000-C000-000000000046}' : AppointmentItem,
	'{00061031-0000-0000-C000-000000000046}' : ContactItem,
	'{00061032-0000-0000-C000-000000000046}' : TaskItem,
	'{00061033-0000-0000-C000-000000000046}' : MailItem,
	'{00063034-0000-0000-C000-000000000046}' : _MailItem,
	'{00063035-0000-0000-C000-000000000046}' : _TaskItem,
	'{00063036-0000-0000-C000-000000000046}' : _TaskRequestItem,
	'{00061037-0000-0000-C000-000000000046}' : JournalItem,
	'{00063038-0000-0000-C000-000000000046}' : _TaskRequestAcceptItem,
	'{00063039-0000-0000-C000-000000000046}' : _TaskRequestDeclineItem,
	'{0006F03A-0000-0000-C000-000000000046}' : Application,
	'{0006303B-0000-0000-C000-000000000046}' : Recipients,
	'{0006103C-0000-0000-C000-000000000046}' : DistListItem,
	'{0006303D-0000-0000-C000-000000000046}' : UserProperties,
	'{0006303E-0000-0000-C000-000000000046}' : Actions,
	'{0006303F-0000-0000-C000-000000000046}' : Pages,
	'{00063040-0000-0000-C000-000000000046}' : _Folders,
	'{00063041-0000-0000-C000-000000000046}' : _Items,
	'{00063042-0000-0000-C000-000000000046}' : UserProperty,
	'{00063043-0000-0000-C000-000000000046}' : Action,
	'{00063044-0000-0000-C000-000000000046}' : RecurrencePattern,
	'{00063045-0000-0000-C000-000000000046}' : Recipient,
	'{00063046-0000-0000-C000-000000000046}' : FormDescription,
	'{00063048-0000-0000-C000-000000000046}' : AddressLists,
	'{00063049-0000-0000-C000-000000000046}' : AddressList,
	'{0006304A-0000-0000-C000-000000000046}' : AddressEntries,
	'{0006304B-0000-0000-C000-000000000046}' : AddressEntry,
	'{0006304C-0000-0000-C000-000000000046}' : Exceptions,
	'{0006304D-0000-0000-C000-000000000046}' : Exception,
	'{0006304E-0000-0000-C000-000000000046}' : ApplicationEvents,
	'{0006304F-0000-0000-C000-000000000046}' : ExplorerEvents,
	'{00063050-0000-0000-C000-000000000046}' : Explorer,
	'{00063051-0000-0000-C000-000000000046}' : Folders,
	'{00063052-0000-0000-C000-000000000046}' : Items,
	'{00063053-0000-0000-C000-000000000046}' : Explorers,
	'{00063054-0000-0000-C000-000000000046}' : Inspectors,
	'{00063055-0000-0000-C000-000000000046}' : OutlookBarPane,
	'{00063056-0000-0000-C000-000000000046}' : OutlookBarGroups,
	'{00063057-0000-0000-C000-000000000046}' : OutlookBarShortcuts,
	'{00063058-0000-0000-C000-000000000046}' : Inspector,
	'{00061060-0000-0000-C000-000000000046}' : RemoteItem,
	'{00061061-0000-0000-C000-000000000046}' : DocumentItem,
	'{00063062-0000-0000-C000-000000000046}' : _MeetingItem,
	'{00063070-0000-0000-C000-000000000046}' : _OutlookBarPane,
	'{00063071-0000-0000-C000-000000000046}' : OutlookBarStorage,
	'{00063072-0000-0000-C000-000000000046}' : _OutlookBarGroups,
	'{00063073-0000-0000-C000-000000000046}' : OutlookBarGroup,
	'{00063074-0000-0000-C000-000000000046}' : _OutlookBarShortcuts,
	'{00063075-0000-0000-C000-000000000046}' : OutlookBarShortcut,
	'{00063076-0000-0000-C000-000000000046}' : FoldersEvents,
	'{00063077-0000-0000-C000-000000000046}' : ItemsEvents,
	'{00063078-0000-0000-C000-000000000046}' : ExplorersEvents,
	'{00063079-0000-0000-C000-000000000046}' : InspectorsEvents,
	'{0006307A-0000-0000-C000-000000000046}' : OutlookBarPaneEvents,
	'{0006307B-0000-0000-C000-000000000046}' : OutlookBarGroupsEvents,
	'{0006307C-0000-0000-C000-000000000046}' : OutlookBarShortcutsEvents,
	'{0006307D-0000-0000-C000-000000000046}' : InspectorEvents,
	'{0006307E-0000-0000-C000-000000000046}' : PropertyPage,
	'{0006307F-0000-0000-C000-000000000046}' : PropertyPageSite,
	'{00063080-0000-0000-C000-000000000046}' : PropertyPages,
	'{00063081-0000-0000-C000-000000000046}' : _DistListItem,
	'{00063083-0000-0000-C000-000000000046}' : _SyncObject,
	'{00063084-0000-0000-C000-000000000046}' : SyncObject,
	'{00063085-0000-0000-C000-000000000046}' : SyncObjectEvents,
	'{00063086-0000-0000-C000-000000000046}' : SyncObjects,
	'{00063087-0000-0000-C000-000000000046}' : Selection,
	'{00063089-0000-0000-C000-000000000046}' : Link,
	'{0006308A-0000-0000-C000-000000000046}' : Links,
	'{0006308B-0000-0000-C000-000000000046}' : NameSpace,
	'{0006308C-0000-0000-C000-000000000046}' : NameSpaceEvents,
	'{D87E7E16-6897-11CE-A6C0-00AA00608FAA}' : _IRecipientControl,
	'{D87E7E17-6897-11CE-A6C0-00AA00608FAA}' : _DRecipientControlEvents,
	'{0006F024-0000-0000-C000-000000000046}' : _DocSiteControl,
	'{0006F025-0000-0000-C000-000000000046}' : _DRecipientControl,
	'{0006F026-0000-0000-C000-000000000046}' : _DDocSiteControl,
	'{00063033-0000-0000-C000-000000000046}' : _AppointmentItem,
	'{00061034-0000-0000-C000-000000000046}' : NoteItem,
	'{00061035-0000-0000-C000-000000000046}' : ReportItem,
	'{00061036-0000-0000-C000-000000000046}' : MeetingItem,
	'{00063037-0000-0000-C000-000000000046}' : _TaskRequestUpdateItem,
	'{0006103A-0000-0000-C000-000000000046}' : PostItem,
	'{0006303A-0000-0000-C000-000000000046}' : ItemEvents,
	'{0006303C-0000-0000-C000-000000000046}' : Attachments,
} CLSIDToPackageMap = {} win32com.client.CLSIDToClass.RegisterCLSIDsFromDict( CLSIDToClassMap ) VTablesToPackageMap = {} VTablesToClassMap = {
	'{00063040-0000-0000-C000-000000000046}' : '_Folders',
	'{00063041-0000-0000-C000-000000000046}' : '_Items',
	'{00063042-0000-0000-C000-000000000046}' : 'UserProperty',
	'{00063043-0000-0000-C000-000000000046}' : 'Action',
	'{00063044-0000-0000-C000-000000000046}' : 'RecurrencePattern',
	'{00063045-0000-0000-C000-000000000046}' : 'Recipient',
	'{00063046-0000-0000-C000-000000000046}' : 'FormDescription',
	'{00063081-0000-0000-C000-000000000046}' : '_DistListItem',
	'{00063048-0000-0000-C000-000000000046}' : 'AddressLists',
	'{00063049-0000-0000-C000-000000000046}' : 'AddressList',
	'{0006304A-0000-0000-C000-000000000046}' : 'AddressEntries',
	'{0006304B-0000-0000-C000-000000000046}' : 'AddressEntry',
	'{0006304C-0000-0000-C000-000000000046}' : 'Exceptions',
	'{0006304D-0000-0000-C000-000000000046}' : 'Exception',
	'{00063083-0000-0000-C000-000000000046}' : '_SyncObject',
	'{00063062-0000-0000-C000-000000000046}' : '_MeetingItem',
	'{00063006-0000-0000-C000-000000000046}' : 'MAPIFolder',
	'{00063007-0000-0000-C000-000000000046}' : 'Attachment',
	'{00063070-0000-0000-C000-000000000046}' : '_OutlookBarPane',
	'{00063071-0000-0000-C000-000000000046}' : 'OutlookBarStorage',
	'{00063072-0000-0000-C000-000000000046}' : '_OutlookBarGroups',
	'{00063073-0000-0000-C000-000000000046}' : 'OutlookBarGroup',
	'{00063074-0000-0000-C000-000000000046}' : '_OutlookBarShortcuts',
	'{00063075-0000-0000-C000-000000000046}' : 'OutlookBarShortcut',
	'{00063089-0000-0000-C000-000000000046}' : 'Link',
	'{0006308A-0000-0000-C000-000000000046}' : 'Links',
	'{0006307F-0000-0000-C000-000000000046}' : 'PropertyPageSite',
	'{00063080-0000-0000-C000-000000000046}' : 'PropertyPages',
	'{00063001-0000-0000-C000-000000000046}' : '_Application',
	'{00063002-0000-0000-C000-000000000046}' : '_NameSpace',
	'{00063003-0000-0000-C000-000000000046}' : '_Explorer',
	'{00063005-0000-0000-C000-000000000046}' : '_Inspector',
	'{00063086-0000-0000-C000-000000000046}' : 'SyncObjects',
	'{00063087-0000-0000-C000-000000000046}' : 'Selection',
	'{00063008-0000-0000-C000-000000000046}' : '_Inspectors',
	'{00063009-0000-0000-C000-000000000046}' : 'Panes',
	'{0006300A-0000-0000-C000-000000000046}' : '_Explorers',
	'{00063020-0000-0000-C000-000000000046}' : '_DocumentItem',
	'{00063021-0000-0000-C000-000000000046}' : '_ContactItem',
	'{00063022-0000-0000-C000-000000000046}' : '_JournalItem',
	'{00063023-0000-0000-C000-000000000046}' : '_RemoteItem',
	'{00063024-0000-0000-C000-000000000046}' : '_PostItem',
	'{00063025-0000-0000-C000-000000000046}' : '_NoteItem',
	'{00063026-0000-0000-C000-000000000046}' : '_ReportItem',
	'{00063033-0000-0000-C000-000000000046}' : '_AppointmentItem',
	'{00063034-0000-0000-C000-000000000046}' : '_MailItem',
	'{00063035-0000-0000-C000-000000000046}' : '_TaskItem',
	'{00063036-0000-0000-C000-000000000046}' : '_TaskRequestItem',
	'{00063037-0000-0000-C000-000000000046}' : '_TaskRequestUpdateItem',
	'{00063038-0000-0000-C000-000000000046}' : '_TaskRequestAcceptItem',
	'{00063039-0000-0000-C000-000000000046}' : '_TaskRequestDeclineItem',
	'{0006303B-0000-0000-C000-000000000046}' : 'Recipients',
	'{0006303C-0000-0000-C000-000000000046}' : 'Attachments',
	'{0006303D-0000-0000-C000-000000000046}' : 'UserProperties',
	'{0006303E-0000-0000-C000-000000000046}' : 'Actions',
	'{0006303F-0000-0000-C000-000000000046}' : 'Pages',
} NamesToIIDMap = {
	'AddressEntry' : '{0006304B-0000-0000-C000-000000000046}',
	'NameSpaceEvents' : '{0006308C-0000-0000-C000-000000000046}',
	'_Application' : '{00063001-0000-0000-C000-000000000046}',
	'ItemsEvents' : '{00063077-0000-0000-C000-000000000046}',
	'SyncObjectEvents' : '{00063085-0000-0000-C000-000000000046}',
	'FormDescription' : '{00063046-0000-0000-C000-000000000046}',
	'ExplorersEvents' : '{00063078-0000-0000-C000-000000000046}',
	'InspectorsEvents' : '{00063079-0000-0000-C000-000000000046}',
	'_Explorers' : '{0006300A-0000-0000-C000-000000000046}',
	'_TaskRequestAcceptItem' : '{00063038-0000-0000-C000-000000000046}',
	'_MailItem' : '{00063034-0000-0000-C000-000000000046}',
	'_OutlookBarShortcuts' : '{00063074-0000-0000-C000-000000000046}',
	'Exceptions' : '{0006304C-0000-0000-C000-000000000046}',
	'_OutlookBarGroups' : '{00063072-0000-0000-C000-000000000046}',
	'PropertyPageSite' : '{0006307F-0000-0000-C000-000000000046}',
	'OutlookBarShortcut' : '{00063075-0000-0000-C000-000000000046}',
	'Attachment' : '{00063007-0000-0000-C000-000000000046}',
	'_TaskRequestDeclineItem' : '{00063039-0000-0000-C000-000000000046}',
	'OutlookBarStorage' : '{00063071-0000-0000-C000-000000000046}',
	'_MeetingItem' : '{00063062-0000-0000-C000-000000000046}',
	'Exception' : '{0006304D-0000-0000-C000-000000000046}',
	'Panes' : '{00063009-0000-0000-C000-000000000046}',
	'OutlookBarPaneEvents' : '{0006307A-0000-0000-C000-000000000046}',
	'Recipients' : '{0006303B-0000-0000-C000-000000000046}',
	'_DDocSiteControl' : '{0006F026-0000-0000-C000-000000000046}',
	'_DistListItem' : '{00063081-0000-0000-C000-000000000046}',
	'MAPIFolder' : '{00063006-0000-0000-C000-000000000046}',
	'AddressEntries' : '{0006304A-0000-0000-C000-000000000046}',
	'_Explorer' : '{00063003-0000-0000-C000-000000000046}',
	'_PostItem' : '{00063024-0000-0000-C000-000000000046}',
	'_DRecipientControlEvents' : '{D87E7E17-6897-11CE-A6C0-00AA00608FAA}',
	'AddressList' : '{00063049-0000-0000-C000-000000000046}',
	'Link' : '{00063089-0000-0000-C000-000000000046}',
	'_Folders' : '{00063040-0000-0000-C000-000000000046}',
	'ApplicationEvents' : '{0006304E-0000-0000-C000-000000000046}',
	'_Items' : '{00063041-0000-0000-C000-000000000046}',
	'Recipient' : '{00063045-0000-0000-C000-000000000046}',
	'Pages' : '{0006303F-0000-0000-C000-000000000046}',
	'_IDocSiteControl' : '{43507DD0-811D-11CE-B565-00AA00608FAA}',
	'InspectorEvents' : '{0006307D-0000-0000-C000-000000000046}',
	'_TaskItem' : '{00063035-0000-0000-C000-000000000046}',
	'_NameSpace' : '{00063002-0000-0000-C000-000000000046}',
	'OutlookBarShortcutsEvents' : '{0006307C-0000-0000-C000-000000000046}',
	'ExplorerEvents' : '{0006304F-0000-0000-C000-000000000046}',
	'_RemoteItem' : '{00063023-0000-0000-C000-000000000046}',
	'_SyncObject' : '{00063083-0000-0000-C000-000000000046}',
	'PropertyPage' : '{0006307E-0000-0000-C000-000000000046}',
	'_AppointmentItem' : '{00063033-0000-0000-C000-000000000046}',
	'ItemEvents' : '{0006303A-0000-0000-C000-000000000046}',
	'OutlookBarGroup' : '{00063073-0000-0000-C000-000000000046}',
	'_DDocSiteControlEvents' : '{50BB9B50-811D-11CE-B565-00AA00608FAA}',
	'AddressLists' : '{00063048-0000-0000-C000-000000000046}',
	'Action' : '{00063043-0000-0000-C000-000000000046}',
	'_JournalItem' : '{00063022-0000-0000-C000-000000000046}',
	'_OutlookBarPane' : '{00063070-0000-0000-C000-000000000046}',
	'_Inspectors' : '{00063008-0000-0000-C000-000000000046}',
	'UserProperties' : '{0006303D-0000-0000-C000-000000000046}',
	'FoldersEvents' : '{00063076-0000-0000-C000-000000000046}',
	'UserProperty' : '{00063042-0000-0000-C000-000000000046}',
	'Selection' : '{00063087-0000-0000-C000-000000000046}',
	'PropertyPages' : '{00063080-0000-0000-C000-000000000046}',
	'Attachments' : '{0006303C-0000-0000-C000-000000000046}',
	'Links' : '{0006308A-0000-0000-C000-000000000046}',
	'_ReportItem' : '{00063026-0000-0000-C000-000000000046}',
	'_Inspector' : '{00063005-0000-0000-C000-000000000046}',
	'_NoteItem' : '{00063025-0000-0000-C000-000000000046}',
	'SyncObjects' : '{00063086-0000-0000-C000-000000000046}',
	'Actions' : '{0006303E-0000-0000-C000-000000000046}',
	'OutlookBarGroupsEvents' : '{0006307B-0000-0000-C000-000000000046}',
	'_ContactItem' : '{00063021-0000-0000-C000-000000000046}',
	'_TaskRequestUpdateItem' : '{00063037-0000-0000-C000-000000000046}',
	'_TaskRequestItem' : '{00063036-0000-0000-C000-000000000046}',
	'_DRecipientControl' : '{0006F025-0000-0000-C000-000000000046}',
	'_DocumentItem' : '{00063020-0000-0000-C000-000000000046}',
	'_IRecipientControl' : '{D87E7E16-6897-11CE-A6C0-00AA00608FAA}',
	'RecurrencePattern' : '{00063044-0000-0000-C000-000000000046}',
} win32com.client.constants.__dicts__.append(constants.__dict__)

