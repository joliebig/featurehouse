from os import name
if name == 'posix':
    from Amarok import Amarok
    from Exaile import Exaile
    from Xmms import Xmms
    from Banshee import Banshee
    from Audacious import Audacious
    from Rhythmbox import Rhythmbox
    from QuodLibet import QuodLibet
    from Listen import Listen
else:
    from Winamp import Winamp
from CurrentSong import CurrentSong
from Mpd import Mpd
del name
