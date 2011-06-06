'''some aditions to the status module to be used by emesene'''
from abstract.status import *
STATUS_TO_MSN = {}
STATUS_TO_MSN[ONLINE] = 'NLN' 
STATUS_TO_MSN[BE_RIGHT_BACK] = 'BRB' 
STATUS_TO_MSN[BUSY] = 'BSY' 
STATUS_TO_MSN[AWAY] = 'AWY' 
STATUS_TO_MSN[TELEPHONE] = 'PHN' 
STATUS_TO_MSN[LUNCH] = 'LUN' 
STATUS_TO_MSN[INVISIBLE] = 'HDN' 
STATUS_TO_MSN[IDLE] = 'IDL' 
STATUS_TO_MSN[OFFLINE] = 'FLN' 
MSN_TO_STATUS = {}
for (key, value) in STATUS_TO_MSN.iteritems():
    MSN_TO_STATUS[value] = key
MSN_TO_STATUS['online'] = ONLINE
MSN_TO_STATUS['brb'] = BE_RIGHT_BACK
MSN_TO_STATUS['busy'] = BUSY
MSN_TO_STATUS['away'] = AWAY
MSN_TO_STATUS['phone'] = TELEPHONE
MSN_TO_STATUS['lunch'] = LUNCH
MSN_TO_STATUS['invisible'] = INVISIBLE
MSN_TO_STATUS['idle'] = IDLE
MSN_TO_STATUS['offline'] = OFFLINE
