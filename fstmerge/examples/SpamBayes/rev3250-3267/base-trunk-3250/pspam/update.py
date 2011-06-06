import getopt
import os
import sys
import pspam.database
from pspam.profile import Profile
from spambayes.Options import options
def folder_exists(L, p):
    """Return true folder with path p exists in list L."""
    for f in L:
        if f.path == p:
            return True
    return False
def main(rebuild=False):
    db = pspam.database.open()
    r = db.open().root()
    profile = r.get("profile")
    if profile is None or rebuild:
        r["profile"] = Profile(options["ZODB", "folder_dir"])
        profile = r["profile"]
        get_transaction().commit()
    for ham in options["ZODB", "ham_folders"].split(os.pathsep):
        p = os.path.join(options["ZODB", "folder_dir"], ham)
        if not folder_exists(profile.hams, p):
            profile.add_ham(p)
    for spam in options["ZODB", "spam_folders"].split(os.pathsep):
        p = os.path.join(options["ZODB", "folder_dir"], spam)
        if not folder_exists(profile.spams, p):
            profile.add_spam(p)
    get_transaction().commit()
    profile.update()
    get_transaction().commit()
    db.close()
if __name__ == "__main__":
    FORCE_REBUILD = False
    opts, args = getopt.getopt(sys.argv[1:], 'F')
    for k, v in opts:
        if k == '-F':
            FORCE_REBUILD = True
    main(FORCE_REBUILD)
