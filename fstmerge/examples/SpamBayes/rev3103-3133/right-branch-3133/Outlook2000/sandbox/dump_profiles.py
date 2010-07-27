from win32com.client import Dispatch
from win32com.mapi import mapi
from win32com.mapi.mapitags import *
mapi.MAPIInitialize(None)
logonFlags = mapi.MAPI_NO_MAIL | mapi.MAPI_EXTENDED
session = mapi.MAPILogonEx(0, None, None, logonFlags)
MAPI_SUBSYSTEM = 39
restriction = mapi.RES_PROPERTY, (mapi.RELOP_EQ, PR_RESOURCE_TYPE, (PR_RESOURCE_TYPE,MAPI_SUBSYSTEM))
table = session.GetStatusTable(0)
rows = mapi.HrQueryAllRows(table,
                            (PR_DISPLAY_NAME_A,),   
                            restriction,     
                            None,            
                            0)               
assert len(rows)==1, "Should be exactly one row"
(tag, val), = rows[0]
print "Profile name:", val
