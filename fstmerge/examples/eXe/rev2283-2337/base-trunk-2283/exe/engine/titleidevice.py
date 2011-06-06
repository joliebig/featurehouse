"""
This module is kept here for backwards compatibility of packages because
Jelly expects to be able to import a TitleIdevice class from a titleidevice 
module.  See node.py:Node.upgradeToVersion2 for more details.
"""
class TitleIdevice:
    """For backwards compatibility of jellied packages only"""
    def __init__(self):
        """Initialize"""
        pass
    def nothing(self):
        """Just to keep PyLint happy"""
        pass
