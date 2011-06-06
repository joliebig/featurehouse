"""
This module contains resource classes used for eXe
"""
import logging
import os
from copy                 import deepcopy
from string               import Template
from exe.engine.persist   import Persistable
from exe.engine.path      import Path, toUnicode 
from exe                       import globals as G
log = logging.getLogger(__name__)
class _Resource(Persistable):
    """
    Encapsulates a resource file which belongs to some package.
    saves the user from having to know if it is renamed.
    This is a user resource, a copy of the file is made and kept with the package.
    Once all resources refering to the file have died, the copy is deleted.
    """
    persistenceVersion = 2
    _package = None
    _idevice = None
    def __init__(self, owner, resourceFile):
        """
        Initialize a resource object, and copy the file into the package's
        resouceDir unless it is already there
        'owner' is either an IDevice or a Package
        'resourceFile' is the path to the file
        """
        log.debug(u"init resourceFile=%s" % resourceFile)
        self._storageName = self._fn2ascii(resourceFile)
        self._userName = resourceFile.encode('utf-8')
        self._originalFile = resourceFile
        try:
            self.checksum = resourceFile.md5
            from exe.engine.idevice   import Idevice 
            from exe.engine.field     import FieldWithResources
            if isinstance(owner, Idevice):
                self._idevice     = owner
                if owner.parentNode:
                    self.package  = owner.parentNode.package
                else:
                    self.package  = None
            elif isinstance(owner, FieldWithResources):
                self._idevice = owner.idevice
                if owner.parentNode:
                    self.package  = owner.parentNode.package
                else:
                    self.package  = None 
            else: 
                self._idevice = None 
                self.package = owner
        finally:
            del self._originalFile
    def _setPackage(self, package):
        """
        Used to change the package.
        """
        if package is self._package: return
        oldPackage = self._package
        if not hasattr(self, 'checksum') or self.checksum is None:
            if package is None:
                log.warn("Resource " + repr(self) + " has no checksum " \
                        + "(probably no source file), but is being removed "\
                        + "anyway, so ignoring.")
            else: 
                if hasattr(self._package, 'resourceDir'):
                    log.warn("Resource " + repr(self) + " has no checksum " \
                        + "(probably old resource), and is being added to "\
                        + "valid package " + repr(package) )
                    log.warn("This resource should have been upgraded first!" \
                            + " Will be ignoring...")
                else:
                    log.warn("Resource " + repr(self) + " has no checksum " \
                        + "(probably no source file), and was being added to "\
                        + "invalid package " + repr(package) 
                        + "; setting to None.")
                self._package = None
            return
        if self._package and hasattr(self._package, 'resources')\
        and self.checksum in self._package.resources:
            siblings = self._package.resources[self.checksum]
            try: 
                while self in siblings: 
                    siblings.remove(self) 
            except Exception, e:
                bogus_condition = 1
            if len(siblings) == 0:
                del self._package.resources[self.checksum]
            oldPath = self.path
        elif hasattr(self, '_originalFile'):
            oldPath = self._originalFile
        else:
            log.warn("Tried to remove a resource (\"" + repr(self)
                    + "\") from what seems to be a "
                    + "corrupt package: \"" + repr(self._package) 
                    + "\"; setting oldPackage to None.")
            oldPath = None
            oldPackage = None
        self._package = package
        if self._package:
            self._addOurselvesToPackage(oldPath)
        if oldPackage and self.checksum not in oldPackage.resources:
            if oldPath and isinstance(oldPath, Path) and oldPath.exists():
                try: 
                    oldPath.remove()
                except WindowsError:
                    pass
            else:
                log.error("Tried to delete a resource that's already not "
                    + " there anymore: filename=\"%s\" "
                    + "userName=\"%s\"" % (oldPath, self.userName))
        if self._idevice \
        and self._idevice.parentNode.package is not self._package:
            self._idevice.userResources.remove(self)
            self._idevice = None
    storageName = property(lambda self:self._storageName)
    userName = property(lambda self:self._userName)
    package = property(lambda self:self._package, _setPackage)
    path = property(lambda self:self._package.resourceDir/self._storageName)
    def _addOurselvesToPackage(self, oldPath):
        """
        Adds ourselves into self._package.resources.
        Don't call if self._package is None.
        Does no copying or anything. Just sticks us in the list and sets our storage name
        """
        if not hasattr(self, 'checksum') or self.checksum is None:
            if self._package is None:
                log.warn("Resource " + repr(self) + " has no checksum " \
                        + "(probably no source file), but is being removed "\
                        + "anyway, so ignoring.")
                return
            else:
                if oldPath.isfile():
                    log.warn("Resource " + repr(self) + " has no checksum; " \
                            + " adding and continuing...")
                    self.checksum = oldPath.md5
                else: 
                    log.warn("Resource " + repr(self) + " has no checksum " \
                        + "(and no source file), and was being added to "\
                        + "package " + repr(self._package) + "; ignoring.")
                    return
        if not hasattr(self._package, 'resources'):
            log.error("_AddOurselvesToPackage called with an invalid package: " 
                    + " no resources on package " + repr(self._package)
                    + "; possibly after a deepcopy")
            return
        if not hasattr(self._package, 'resourceDir'):
            log.error("_AddOurselvesToPackage called with an invalid package: " 
                    + " no resourceDir on package " + repr(self._package)
                    + "; possibly an old/corrupt resource or package")
            return
        siblings = self._package.resources.setdefault(self.checksum, [])
        if siblings:
            newName = siblings[0]._storageName
            if oldPath.dirname() == self._package.resourceDir and self._storageName != newName:
                oldPath.remove()
            self._storageName = newName
        else:
            if Path(oldPath).dirname() == self._package.resourceDir:
                log.debug(u"StorageName=%s was already in self._package resources" % self._storageName)
            else:
                filename = (self._package.resourceDir/oldPath.basename())
                storageName = self._fn2ascii(filename)
                storageName = (self._package.resourceDir/storageName).unique()
                self._storageName = str(storageName.basename())
                oldPath.copyfile(self.path)
        if self not in siblings:
            siblings.append(self)
    def delete(self):
        """
        Remove a resource from a package
        """
        self.package = None
    def __unicode__(self):
        """
        return the string
        """
        return self._storageName
    def __getinitargs__NOT__(self):
        """
        Used by copy.deepcopy, which is used by exe.engine.node.clone().
        Makes it so the copy for this resource, actually gets __init__ called
        """
        if self._idevice:
            return self._idevice, self.path
        else:
            return self._package, self.path
    def __deepcopy__(self, others={}):
        """
        Returns a copy of self, letting our package and idevice know what has happened
        'others' is the dict of id, object of everything that's been copied already
        """
        miniMe = self.__class__.__new__(self.__class__)
        others[id(self)] = miniMe
        for key, val in self.__dict__.items():
            if id(val) in others:
                setattr(miniMe, key, others[id(val)])
            else:
                new = deepcopy(val, others)
                others[id(val)] = new
                setattr(miniMe, key, new)
        if miniMe.package:
            miniMe._addOurselvesToPackage(self.path)
        return miniMe
    def _fn2ascii(self, filename):
        """
        Changes any filename to pure ascii, returns only the basename
        """     
        nameBase, ext = Path(Path(filename).basename()).splitext()
        try: nameBase.encode('ascii')
        except UnicodeEncodeError:
            nameBase = nameBase.encode('utf-8').encode('hex')
        try:
            ext = ext.encode('ascii')
        except UnicodeEncodeError:
            ext = ext.encode('utf8').encode('hex')
        return str(nameBase + ext)
class Resource(_Resource):
    """
    This is a user resource, a copy of the file is made and kept with the package.
    Once all resources refering to the file have died, the copy is deleted.
    """
    persistenceVersion = 2
    def __init__(self, owner, resourceFile):
        """
        Initialize a resource object, and copy the file into the package's
        resouceDir unless it is already there
        'owner' is either an IDevice or a Package
        'resourceFile' is the path to the file
        """
        self.checksum = Path(resourceFile).md5 # Just use the path name as a unique ID
        _Resource.__init__(self, owner, resourceFile)
        if self._idevice:
            self._idevice.userResources.append(self)
    @property
    def path(self):
        """
        Returns the path to the resource
        """
        if hasattr(self, '_package') and self._package\
        and hasattr(self._package, 'resourceDir'):
            return self._package.resourceDir/self._storageName
        else:
            return self._storageName
    def checksumCheck(self):
        """
        Ensures the the md5 is correct.             
        There was a period in which resource checksums were being created 
        before the resource zip file was fully closed, and not flushed out
        """
        try: 
            if self.path.isfile(): 
                new_md5 = self.path.md5 
            else: 
                new_md5 = None
        except Exception, e:
            bogus_condition = 1
            found_resource = False
            this_resource_path = ""
            if hasattr(self, '_idevice') and self._idevice is not None:
                if hasattr(self._idevice, 'parentNode') \
                and self._idevice.parentNode is not None:
                    if hasattr(self._idevice.parentNode, 'package')\
                    and self._idevice.parentNode.package is not None:
                        if hasattr(self._idevice.parentNode.package, \
                                'resourceDir')\
                        and self._idevice.parentNode.package.resourceDir \
                        is not None: 
                            this_resource_path = \
                                self._idevice.parentNode.package.resourceDir \
                                + "/" + self._storageName
                            if os.path.isfile(this_resource_path):
                                found_resource = True
                                new_md5 = this_resource_path.md5
                                self._package = self._idevice.parentNode.package
            if not found_resource:
                new_md5 = None 
                log.warn('Failed to do a checksumCheck on resource ' 
                    + repr(self)
                    + ', with unknown path, but storageName = ' 
                    + repr(self._storageName)) 
            else:
                log.warn('checksumCheck not able to find resource path '
                        + 'directly, since no package, but did find a path '
                        + 'to it at: ' + this_resource_path)
        if not hasattr(self, 'checksum'):
            log.warn("checksumCheck() found NO checksum attribute for " 
                    + repr(self) + "; setting to new md5 of: " + str(new_md5))
            self.checksum = new_md5
            if new_md5 is not None and hasattr(self._package, 'resources'): 
                siblings = self._package.resources.setdefault(new_md5, [])
                if self not in siblings:
                    siblings.append(self)
        elif self.checksum != new_md5: 
            old_md5 = self.checksum
            log.warn("checksumCheck() found old md5 for " + repr(self) 
                    + "; replacing with: " + str(new_md5)) 
            self.checksum = new_md5
            if hasattr(self._package, 'resources'): 
                siblings = self._package.resources[old_md5]
                while self in siblings: 
                    siblings.remove(self)
                if len(siblings) == 0:
                    del self._package.resources[old_md5]
                if new_md5 is not None: 
                    siblings = self._package.resources.setdefault(new_md5, [])
                    if self not in siblings:
                        siblings.append(self)
    def upgradeToVersion2(self):
        """
        a wrapper to addSelfToPackageList(self), such that it might be called
        after the package has been loaded and upgraded.  Otherwise, due 
        to the seemingly random upgrading of the package and resource objects,
        this might be called too early.
        """
        G.application.afterUpgradeHandlers.append(self.addSelfToPackageList)
    def addSelfToPackageList(self):
        """
        For upgradeToVersion2, to version 0.20 of exe.
        Puts user resources in our package's list
        """
        if self._package:
            if not hasattr(self._package, 'resourceDir'):
                log.warn("resource " + repr(self) + " in addSelfToPackage with "
                        + "invalid self._package = \"" + self._package._name 
                        + "\" " + repr(self._package) 
                        + ". Setting to None and returning")
                self._package = None
                return
            if not hasattr(self._package, 'resources'):
                self._package.resources = {}
        elif self._package is None:
            log.warn("resource " + repr(self) + " in addSelfToPackageList with "
                    + "self._package = None (perhaps a zombie already deleted?). "
                    + "Returning.")
            return
        self._userName = self._storageName
        if self.path.isfile():
            self.checksum = self.path.md5
            self._originalFile = self.path  # Pretend we're a newly added file
            try:
                self._addOurselvesToPackage(self.path)
            finally:
                del self._originalFile
        else:
            log.error('Resource file "%s" not found. Deleting resource object' % self.path)
            if self._idevice:
                self._idevice.userResources.remove(self)
    def __repr__(self):
        """
        Represents 'Resource' as a string for the programmer
        """
        return '<%s.%s for "%s" at %s>' % (__name__, self.__class__.__name__, self._storageName, id(self))
    def renameForMerging(self, newPackage):
        """
        to help the merging code in twisted/persisted/styles.py
        when it finds that this resource's storageName is already 
        in use with the merging destination package, newPackage.
        Finds a new unique name by prepending "m1_", "m2_", etc...
        note: new name should, of course, be unique to both the resource's
        current source package, as well as the destinateion newPackage.
        """ 
        old_name = self._storageName
        found_new_name = False
        new_name_attempt = 0
        preMergePackage = self.package
        self.checksumCheck()
        if self.checksum in newPackage.resources:
            if self._storageName  \
            == newPackage.resources[self.checksum][0]._storageName:
                found_new_name = False
            else:
                new_name = newPackage.resources[self.checksum][0]._storageName
                found_new_name = True
        while not found_new_name: 
            new_name_attempt += 1 
            new_name_suffix = "m" \
                + str(new_name_attempt) + "_"
            new_name = new_name_suffix \
                + self.storageName 
            if newPackage.findResourceByName(new_name) is None \
            and preMergePackage.findResourceByName(new_name) is None:
                found_new_name = True
        if found_new_name:
            log.warn("Renaming Resource from \""
                    + self.storageName + "\" to \""
                    + new_name)
            old_path = Path(preMergePackage.resourceDir)\
                    .joinpath(self.storageName) 
            new_path = Path(preMergePackage.resourceDir)\
                    .joinpath(new_name)
            newFullFileName = Path(old_path).rename(new_path)
            self._storageName = new_name
            from exe.engine.appletidevice    import AppletIdevice
            from exe.engine.galleryidevice   import GalleryIdevice
            if isinstance(self._idevice, AppletIdevice):
                raise Exception(_(u"renamed a Resource for an Applet Idevice, and it should not have even made it this far!"))
            elif isinstance(self._idevice, GalleryIdevice):
                if self._idevice._createHTMLPopupFile not in \
                G.application.afterUpgradeHandlers:
                    G.application.afterUpgradeHandlers.append(
                        self._idevice._createHTMLPopupFile)
            else:
                this_field = self._idevice.getResourcesField(self)
                from exe.engine.field            import FieldWithResources
                if this_field is not None\
                and isinstance(this_field, FieldWithResources):
                    old_resource_path = "resources/" + old_name
                    new_resource_path = "resources/" + new_name
                    this_field.content_w_resourcePaths = \
                        this_field.content_w_resourcePaths.replace(
                            old_resource_path, new_resource_path)
                    if hasattr(this_field, 'content'):
                        this_field.content = this_field.content_w_resourcePaths
                    if hasattr(this_field, 'content_wo_resourcePaths'):
                        this_field.content_wo_resourcePaths = \
                                this_field.MassageContentForRenderView()
                else:
                    log.warn("Unaware of any other specific resource-renaming "
                            + "activities necessary for this type of idevice: "
                            + repr(self._idevice))
    def launch_testForZombies(self):
        """
        a wrapper to testForZombieResources(self), such that it might be called
        after the package has been loaded and upgraded.  Otherwise, due 
        to the seemingly random upgrading of the package and resource objects,
        this might be called too early.
        """
        if self._idevice is None:
            G.application.afterUpgradeHandlers.append(
                    self.testForZombieResources)
    def testForAndDeleteZombieResources(self):
        """
        A quick wrapper around testForZombieResources to force the
        deleteZombie parameter to True (normally defaults to False).
        This additional wrapper is here such that the afterUpgradeHandler
        can call this directly, and not require any particular parameters.
        """
        self.testForZombieResources(deleteZombie=True)
    def testForZombieResources(self, deleteZombie=False):
        """ 
        testing a possible post-load confirmation that this resource 
        is indeed attached to something.  
        to be called from twisted/persist/styles.py upon load of a Resource.
        to accomodate random loading issues, in which, for example, two
        resource objects pointing to the same file are loaded, and one is
        a zombie to be deleted - we want to ensure that the valid one has
        been found and properly re-attach all non-zombies before actually 
        deleting any resources and inadvertently deleting the resource file!
        With a new deleteZombie parameter, this supports a first-pass call
        which merely tries to re-attach any potential zombie resources.
        If STILL a zombie, then the launched 2nd-pass test can delete it.
        """
        self.checksumCheck()
        if self._package is not None\
        and not hasattr(self._package, 'resources'):
            if not deleteZombie:
                log.warn("1st pass: Checking zombie Resource \"" + str(self) 
                    + "\", but package does not yet have resources;"
                    + " ignoring for now.")
                G.application.afterUpgradeHandlers.append(
                    self.testForAndDeleteZombieResources)
                return
            else:
                log.warn("2nd pass: Checking zombie Resource \"" + str(self) 
                    + "\", but package does not yet have resources. deleting.")
                G.application.afterUpgradeZombies2Delete.append(self)
                return
        if self._package is None \
        or self.checksum not in self._package.resources:
            if deleteZombie: 
                log.warn("Removing zombie Resource \"" + str(self) 
                        + "\"; not in package resources.") 
                G.application.afterUpgradeZombies2Delete.append(self)
            else:
                log.warn("1st pass: not yet removing zombie Resource \""
                        + str(self) + "\"; not in package resources.")
                G.application.afterUpgradeHandlers.append(
                    self.testForAndDeleteZombieResources)
                return
        elif self._package is not None and self._idevice is None\
        and self != self._package._backgroundImg:
            found_idevice = None
            if self._package.root: 
                root_and_children = list(self._package.root.walkDescendants())
                root_and_children.append(self._package.root)
                for this_node in root_and_children:
                    for this_idevice in this_node.idevices:
                        just_found_idevice = False
                        if self in this_idevice.userResources:
                            just_found_idevice = True
                        else: 
                            this_field = this_idevice.getResourcesField(self) 
                            if this_field is not None: 
                                just_found_idevice = True
                        if just_found_idevice:
                            if found_idevice:
                                log.warn("Multiple idevices found for "
                                        + " non-zombie Resource \"" + str(self)
                                        + "\" when re-attaching.")
                            just_found_idevice = True
                            found_idevice = this_idevice
                            found_idevice.userResources.append(self)
                            self._idevice = found_idevice
                            log.warn("Re-attached non-zombie Resource \""
                                    + str(self) + "\" to iDevice: "
                                    + repr(found_idevice))
            if found_idevice is None:
                if deleteZombie: 
                    log.warn("Removing zombie Resource \"" + str(self) 
                            + "\"; no corresponding iDevice found.") 
                    G.application.afterUpgradeZombies2Delete.append(self)
                else: 
                    log.warn("1st pass: not yet removing zombie Resource \""
                        + str(self) + "\"; no corresponding iDevice found.")
                    G.application.afterUpgradeHandlers.append(
                        self.testForAndDeleteZombieResources)
