"""
PackageStore is responsible for managing the Packages which the eXe server
has loaded, (and loading and saving them?)
"""
import logging
from exe.engine.package      import Package
log = logging.getLogger(__name__)
class PackageStore:
    """
    PackageStore is responsible for managing the Packages which the eXe server
    has loaded, and loading and saving them
    """
    def __init__(self):
        self.loaded       = {}
    def createPackage(self):
        """
        Creates a package
        """
        log.debug(u"createPackage")
        i = 1
        name = u"newPackage"
        while name in self.loaded:
            name = u"newPackage" + unicode(i)
            i += 1                    
        package = Package(name)
        self.loaded[package.name] = package
        return package
    def getPackage(self, name):
        """
        Get package using the name
        """
        return self.loaded[name]
    def addPackage(self, package):
        """
        Add a package
        """
        self.loaded[package.name] = package
    def saveAll(self):
        """
        Save all the packages in the package store out to disk
        """
        for package in self.loaded.values():
            package.save()
    def loadPackage(self, path):
        """
        Load a package from disk, add it to the store and return it
        """
        package = Package.load(path)
        self.loaded[package.name] = package
        return package
