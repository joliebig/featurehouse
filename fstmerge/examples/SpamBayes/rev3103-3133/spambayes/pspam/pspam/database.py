from spambayes.Options import options

import ZODB

from ZEO.ClientStorage import ClientStorage

import zLOG

import os

def logging():

    os.environ["STUPID_LOG_FILE"] = options["ZODB", "event_log_file"]

    os.environ["STUPID_LOG_SEVERITY"] = str(options["ZODB",
                                                    "event_log_severity"])

    zLOG.initialize()
 def open():

    addr = options["ZODB", "zeo_addr"]

    if addr and addr[0] == "(" and addr[-1] == ")":

        s, p = tuple(addr[1:-1].split(',', 1))

        addr = s, int(p)

    cs = ClientStorage(addr)

    db = ZODB.DB(cs, cache_size=options["ZODB", "cache_size"])

    return db


