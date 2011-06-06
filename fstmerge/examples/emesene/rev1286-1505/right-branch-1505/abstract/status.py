'''a module to handle abstract status for different IM clients'''
OFFLINE = 0
ONLINE = 1
BUSY = 2
AWAY = 3
IDLE = 4
INVISIBLE = 5
LUNCH = 6
TELEPHONE = 7
BE_RIGHT_BACK = 8
ORDERED = [ONLINE, BUSY, AWAY, BE_RIGHT_BACK, IDLE, LUNCH, TELEPHONE,
    INVISIBLE, OFFLINE]
STATUS = {OFFLINE : _('Offline'),
    ONLINE : _('Online'),
    BUSY : _('Busy'),
    AWAY : _('Away'),
    IDLE : _('Idle'),
    INVISIBLE : _('Invisible'),
    LUNCH : _('Lunch'),
    TELEPHONE : _('On the phone'),
    BE_RIGHT_BACK : _('Be right back')}
REVERSE = dict([(y, x) for x, y in STATUS.items()])
def is_valid(status):
    '''return True if status is a valid status value'''
    return status in STATUS
