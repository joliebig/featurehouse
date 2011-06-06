"""Microsoft Add-In Designer"""
makepy_version = '0.4.95'
python_version = 0x20404f0
import win32com.client.CLSIDToClass, pythoncom
import win32com.client.util
from pywintypes import IID
from win32com.client import Dispatch
defaultNamedOptArg=pythoncom.Empty
defaultNamedNotOptArg=pythoncom.Empty
defaultUnnamedArg=pythoncom.Empty
CLSID = IID('{AC0714F2-3D04-11D1-AE7D-00A0C90F26F4}')
MajorVersion = 1
MinorVersion = 0
LibraryFlags = 8
LCID = 0x0
class constants:
	ext_cm_AfterStartup           =0x0        # from enum ext_ConnectMode
	ext_cm_CommandLine            =0x3        # from enum ext_ConnectMode
	ext_cm_External               =0x2        # from enum ext_ConnectMode
	ext_cm_Startup                =0x1        # from enum ext_ConnectMode
	ext_dm_HostShutdown           =0x0        # from enum ext_DisconnectMode
	ext_dm_UserClosed             =0x1        # from enum ext_DisconnectMode
from win32com.client import DispatchBaseClass
class IAddinDesigner(DispatchBaseClass):
	"""Add-In Designer Control"""
	CLSID = IID('{AC0714F3-3D04-11D1-AE7D-00A0C90F26F4}')
	coclass_clsid = IID('{AC0714F6-3D04-11D1-AE7D-00A0C90F26F4}')
	_prop_map_get_ = {
	}
	_prop_map_put_ = {
	}
class IAddinInstance(DispatchBaseClass):
	"""Add-In Instance Object"""
	CLSID = IID('{AC0714F4-3D04-11D1-AE7D-00A0C90F26F4}')
	coclass_clsid = IID('{AC0714F7-3D04-11D1-AE7D-00A0C90F26F4}')
	_prop_map_get_ = {
	}
	_prop_map_put_ = {
	}
class _IDTExtensibility2:
	CLSID = CLSID_Sink = IID('{B65AD801-ABAF-11D0-BB8B-00A0C90F2744}')
	coclass_clsid = IID('{AC0714F7-3D04-11D1-AE7D-00A0C90F26F4}')
	_public_methods_ = [] # For COM Server support
	_dispid_to_func_ = {
		        5 : "OnBeginShutdown",
		        4 : "OnStartupComplete",
		1610678275 : "OnInvoke",
		        3 : "OnAddInsUpdate",
		        1 : "OnConnection",
		1610678273 : "OnGetTypeInfo",
		1610612737 : "OnAddRef",
		1610612736 : "OnQueryInterface",
		        2 : "OnDisconnection",
		1610612738 : "OnRelease",
		1610678274 : "OnGetIDsOfNames",
		1610678272 : "OnGetTypeInfoCount",
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
from win32com.client import CoClassBaseClass
class AddinDesigner(CoClassBaseClass): # A CoClass
	CLSID = IID('{AC0714F6-3D04-11D1-AE7D-00A0C90F26F4}')
	coclass_sources = [
		_IDTExtensibility2,
	]
	default_source = _IDTExtensibility2
	coclass_interfaces = [
		IAddinDesigner,
	]
	default_interface = IAddinDesigner
class AddinInstance(CoClassBaseClass): # A CoClass
	CLSID = IID('{AC0714F7-3D04-11D1-AE7D-00A0C90F26F4}')
	coclass_sources = [
		_IDTExtensibility2,
	]
	default_source = _IDTExtensibility2
	coclass_interfaces = [
		IAddinInstance,
	]
	default_interface = IAddinInstance
IAddinDesigner_vtables_dispatch_ = 1
IAddinDesigner_vtables_ = [
]
IAddinInstance_vtables_dispatch_ = 1
IAddinInstance_vtables_ = [
]
_IDTExtensibility2_vtables_dispatch_ = 1
_IDTExtensibility2_vtables_ = [
	(( 'OnConnection' , 'Application' , 'ConnectMode' , 'AddInInst' , 'custom' , 
			), 1, (1, (), [ (9, 1, None, None) , (3, 1, None, None) , (9, 1, None, None) , (24588, 1, None, None) , ], 1 , 1 , 4 , 0 , 28 , (3, 0, None, None) , 0 , )),
	(( 'OnDisconnection' , 'RemoveMode' , 'custom' , ), 2, (2, (), [ (3, 1, None, None) , 
			(24588, 1, None, None) , ], 1 , 1 , 4 , 0 , 32 , (3, 0, None, None) , 0 , )),
	(( 'OnAddInsUpdate' , 'custom' , ), 3, (3, (), [ (24588, 1, None, None) , ], 1 , 1 , 4 , 0 , 36 , (3, 0, None, None) , 0 , )),
	(( 'OnStartupComplete' , 'custom' , ), 4, (4, (), [ (24588, 1, None, None) , ], 1 , 1 , 4 , 0 , 40 , (3, 0, None, None) , 0 , )),
	(( 'OnBeginShutdown' , 'custom' , ), 5, (5, (), [ (24588, 1, None, None) , ], 1 , 1 , 4 , 0 , 44 , (3, 0, None, None) , 0 , )),
]
RecordMap = {
}
CLSIDToClassMap = {
	'{AC0714F6-3D04-11D1-AE7D-00A0C90F26F4}' : AddinDesigner,
	'{AC0714F7-3D04-11D1-AE7D-00A0C90F26F4}' : AddinInstance,
	'{B65AD801-ABAF-11D0-BB8B-00A0C90F2744}' : _IDTExtensibility2,
	'{AC0714F3-3D04-11D1-AE7D-00A0C90F26F4}' : IAddinDesigner,
	'{AC0714F4-3D04-11D1-AE7D-00A0C90F26F4}' : IAddinInstance,
}
CLSIDToPackageMap = {}
win32com.client.CLSIDToClass.RegisterCLSIDsFromDict( CLSIDToClassMap )
VTablesToPackageMap = {}
VTablesToClassMap = {
	'{B65AD801-ABAF-11D0-BB8B-00A0C90F2744}' : '_IDTExtensibility2',
	'{AC0714F3-3D04-11D1-AE7D-00A0C90F26F4}' : 'IAddinDesigner',
	'{AC0714F4-3D04-11D1-AE7D-00A0C90F26F4}' : 'IAddinInstance',
}
NamesToIIDMap = {
	'IAddinInstance' : '{AC0714F4-3D04-11D1-AE7D-00A0C90F26F4}',
	'_IDTExtensibility2' : '{B65AD801-ABAF-11D0-BB8B-00A0C90F2744}',
	'IAddinDesigner' : '{AC0714F3-3D04-11D1-AE7D-00A0C90F26F4}',
}
win32com.client.constants.__dicts__.append(constants.__dict__)
