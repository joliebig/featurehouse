"""
Different styles of persisted objects.
"""
import types
import copy_reg
import copy
try:
    import cStringIO as StringIO
except ImportError:
    import StringIO
from exe                import globals as G
from exe.engine.path    import Path
import logging
log = logging.getLogger(__name__)
try:
    from new import instancemethod
except:
    from org.python.core import PyMethod
    instancemethod = PyMethod
oldModules = {}
def pickleMethod(method):
    'support function for copy_reg to pickle method refs'
    return unpickleMethod, (method.im_func.__name__,
                             method.im_self,
                             method.im_class)
def unpickleMethod(im_name,
                    im_self,
                    im_class):
    'support function for copy_reg to unpickle method refs'
    try:
        unbound = getattr(im_class,im_name)
        if im_self is None:
            return unbound
        bound=instancemethod(unbound.im_func,
                                 im_self,
                                 im_class)
        return bound
    except AttributeError:
        log.error("Method" + im_name + "not on class" + im_class)
        assert im_self is not None,"No recourse: no instance to guess from."
        unbound = getattr(im_self.__class__,im_name)
        log.error("Attempting fixup with" + unbound)
        if im_self is None:
            return unbound
        bound=instancemethod(unbound.im_func,
                                 im_self,
                                 im_self.__class__)
        return bound
copy_reg.pickle(types.MethodType,
                pickleMethod,
                unpickleMethod)
def pickleModule(module):
    'support function for copy_reg to pickle module refs'
    return unpickleModule, (module.__name__,)
def unpickleModule(name):
    'support function for copy_reg to unpickle module refs'
    if oldModules.has_key(name):
        log.info("Module has moved: " + name)
        name = oldModules[name]
        log.info(name)
    return __import__(name,{},{},'x')
copy_reg.pickle(types.ModuleType,
                pickleModule,
                unpickleModule)
def pickleStringO(stringo):
    'support function for copy_reg to pickle StringIO.OutputTypes'
    return unpickleStringO, (stringo.getvalue(), stringo.tell())
def unpickleStringO(val, sek):
    x = StringIO.StringIO()
    x.write(val)
    x.seek(sek)
    return x
if hasattr(StringIO, 'OutputType'):
    copy_reg.pickle(StringIO.OutputType,
                    pickleStringO,
                    unpickleStringO)
def pickleStringI(stringi):
    return unpickleStringI, (stringi.getvalue(), stringi.tell())
def unpickleStringI(val, sek):
    x = StringIO.StringIO(val)
    x.seek(sek)
    return x
if hasattr(StringIO, 'InputType'):
    copy_reg.pickle(StringIO.InputType,
                pickleStringI,
                unpickleStringI)
class Ephemeral:
    """
    This type of object is never persisted; if possible, even references to it
    are eliminated.
    """
    def __getstate__(self):
        log.warn( "WARNING: serializing ephemeral " + self )
        import gc
        for r in gc.get_referrers(self):
            log.warn( " referred to by " + (r,))
        return None
    def __setstate__(self, state):
        log.warn( "WARNING: unserializing ephemeral " + self.__class__ )
        self.__class__ = Ephemeral
versionedsToUpgrade = {}
upgraded = {}
def doUpgrade(newPackage=None, isMerge=False, preMergePackage=None):
    global versionedsToUpgrade, upgraded
    try: 
        if isMerge:
            log.debug("doUpgrade performing a pre-Merge safety check.")
            for versioned in versionedsToUpgrade.values(): 
                requireUpgrade(versioned, newPackage, 
                    isMerge, preMergePackage, mergeCheck=True)
            log.debug("doUpgrade completed the pre-Merge safety check.")
        for versioned in versionedsToUpgrade.values(): 
            requireUpgrade(versioned, newPackage, 
                    isMerge, preMergePackage, mergeCheck=False)
    except Exception, exc:
        versionedsToUpgrade = {}
        upgraded = {}
        raise
    versionedsToUpgrade = {}
    upgraded = {}
def requireUpgrade(obj, newPackage=None, 
        isMerge=False, preMergePackage=None, mergeCheck=False):
    """Require that a Versioned instance be upgraded completely first.
    """
    objID = id(obj)
    if objID in versionedsToUpgrade and objID not in upgraded:
        if not mergeCheck: 
            upgraded[objID] = 1
        obj.versionUpgrade(newPackage, isMerge, preMergePackage, mergeCheck)
        return obj
from twisted.python import reflect
def _aybabtu(c):
    l = []
    for b in reflect.allYourBase(c, Versioned):
        if b not in l and b is not Versioned:
            l.append(b)
    return l
class Versioned:
    """
    This type of object is persisted with versioning information.
    I have a single class attribute, the int persistenceVersion.  After I am
    unserialized (and styles.doUpgrade() is called), self.upgradeToVersionX()
    will be called for each version upgrade I must undergo.
    For example, if I serialize an instance of a Foo(Versioned) at version 4
    and then unserialize it when the code is at version 9, the calls::
      self.upgradeToVersion5()
      self.upgradeToVersion6()
      self.upgradeToVersion7()
      self.upgradeToVersion8()
      self.upgradeToVersion9()
    will be made.  If any of these methods are undefined, a warning message
    will be printed.
    """
    persistenceVersion = 0
    persistenceForgets = ()
    def __setstate__(self, state):
        if not G.application.persistNonPersistants: 
            versionedsToUpgrade[id(self)] = self
        self.__dict__ = state
    def __getstate__(self, dict=None):
        """Get state, adding a version number to it on its way out.
        """
        dct = copy.copy(dict or self.__dict__)
        bases = _aybabtu(self.__class__)
        bases.reverse()
        bases.append(self.__class__) # don't forget me!!
        for base in bases:
            if base.__dict__.has_key('persistenceForgets'):
                for slot in base.persistenceForgets:
                    if dct.has_key(slot):
                        del dct[slot]
            if base.__dict__.has_key('persistenceVersion'):
                dct['%s.persistenceVersion' % reflect.qual(base)] = base.persistenceVersion
        return dct
    def versionUpgrade(self, newPackage=None, 
                       isMerge=False, preMergePackage=None, mergeCheck=False):
        """(internal) Do a version upgrade.
        """
        bases = _aybabtu(self.__class__)
        bases.reverse()
        bases.append(self.__class__) # don't forget me!!
        if self.__dict__.has_key("persistenceVersion"):
            pver = self.__dict__['persistenceVersion']
            del self.__dict__['persistenceVersion']
            highestVersion = 0
            highestBase = None
            for base in bases:
                if not base.__dict__.has_key('persistenceVersion'):
                    continue
                if base.persistenceVersion > highestVersion:
                    highestBase = base
                    highestVersion = base.persistenceVersion
            if highestBase:
                self.__dict__['%s.persistenceVersion' % reflect.qual(highestBase)] = pver
        for base in bases:
            if repr(base)=="<class 'exe.engine.resource.Resource'>":
                if not mergeCheck: 
                    log.debug("LOADING RESOURCE = \"" + repr(self) + "\"")
                if newPackage is not None and self._package != newPackage:
                    if self._package is not None: 
                        if isMerge and self._package == preMergePackage:
                            if not hasattr(self, 'checksum'):
                                log.error("Old package: unable to "
                                        + "relink old Resource (w/o checksum) "
                                        + repr(self)
                                        + " to new merge package. "
                                        + " Please upgrade package first!")
                                raise Exception(_(u"Package is old. Please upgrade it (using File..Open followed by File..Save As) before attempting to insert it into another package!"))
                        elif isMerge:
                            log.error("Old package: unable to "
                                    + "relink old Resource (w/ wrong package) "
                                    + repr(self)
                                    + " to new merge package. "
                                    + " Please upgrade package first!")
                            raise Exception(_(u"Package is old. Please upgrade it (using File..Open followed by File..Save As) before attempting to insert it into another package!"))
                        else:
                            if not mergeCheck:
                                log.warn("NOT relinking corrupt Resource " \
                                    +repr(self) + " to new package."
                                    + " (letting zombie check correct it)") 
                    else:
                        log.debug("ignoring Resource "+repr(self) \
                            + " as it no longer applies to any package.") 
                if mergeCheck:
                    if self.checksum in newPackage.resources:
                        log.warn("this Resource \"" + self._storageName
                            + "\" already exists in the "
                            + "destination merge package")
                        existing_name = newPackage.resources[self.checksum][0]\
                                ._storageName
                        if self._storageName == existing_name:
                            log.warn(".... and shares the same name.  easy!")
                        else:
                            log.warn(".... but Resource \"" 
                                    + self._storageName
                                    + "\" was called \"" + existing_name
                                    + "\" in the destination"
                                    + " package.")
                            from exe.engine.appletidevice import AppletIdevice
                            if not isinstance(self._idevice, AppletIdevice):
                                self.renameForMerging(newPackage)
                            else:
                                log.error("Unable to merge duplicate resource "
                                    + "with different name in iDevice = "
                                    + repr(self._idevice)
                                    + ", Node = " 
                                    + self._idevice.parentNode.title
                                    + ", name: \""
                                    + self.storageName
                                    + "\", and original name = \""
                                    + existing_name + "\".")
                    else: 
                        this_res = newPackage.findResourceByName(
                                self.storageName)
                        if this_res is not None \
                        and self.storageName == this_res.storageName:
                            log.warn("merging into package that already"
                                + " has another resource of this name.")
                            from exe.engine.appletidevice import AppletIdevice
                            if not isinstance(self._idevice, AppletIdevice):
                                self.renameForMerging(newPackage)
                            else:
                                log.error("Unable to merge duplicate resource "
                                        + "name in iDevice = "
                                        + repr(self._idevice)
                                        + ", Node = " 
                                        + self._idevice.parentNode.title
                                        + ", name: \""
                                        + self.storageName
                                        + "\".")
                elif isMerge:
                    if (self.checksum in newPackage.resources \
                    and self.storageName != \
                    newPackage.resources[self.checksum][0]._storageName) \
                    or ( not self.checksum in newPackage.resources \
                    and newPackage.findResourceByName(self.storageName) \
                    is not None):
                        raise Exception(_(u"Unable to merge: duplicate Java Applet resource names exist (including: \"" + self.storageName + "\"). Please see the log file for the names of ALL such problem resources."))
                if not mergeCheck: 
                    if base.__dict__.has_key("launch_testForZombies"): 
                        method = base.__dict__.get("launch_testForZombies") 
                        method(self)
            elif repr(base) == "<class 'exe.engine.node.Node'>":
                if not mergeCheck: 
                    log.debug("LOADING NODE = \"" + self.getTitle() + "\", "
                         + "nodeId=" + str(self.getId()) + ", @ \"" 
                         + str(id(self)) + "\"")
                if hasattr(self, '_package') \
                and newPackage is not None and self._package != newPackage:
                    if isMerge and self._package != preMergePackage:
                        log.error("Old package: unable to "
                                + "relink old Node (w/ wrong package) "
                                + repr(self)
                                + " to new merge package. "
                                + " Please upgrade package first!")
                        raise Exception(_(u"Package is old. Please upgrade it (using File..Open followed by File..Save As) before attempting to insert it into another package!"))
                    elif not isMerge:
                        log.debug("TRY relinking Node \""+ self.getTitle() 
                            + "\""  +" to new package." 
                            + " (and zombie check will later confirm it)") 
                        self._package = newPackage
                if not mergeCheck: 
                    if base.__dict__.has_key("launch_testForZombies"): 
                        method = base.__dict__.get("launch_testForZombies") 
                        method(self)
            elif repr(base)=="<class 'exe.engine.package.Package'>":
                if not mergeCheck: 
                    log.debug("LOADING PACKAGE = \"" + self._name + "\"")
                if newPackage is not None and self != newPackage:
                    if not mergeCheck:
                        log.debug("ignoring old Package object \"" 
                            + self._name + "\" " + repr(self))
            if (Versioned not in base.__bases__ and
                not base.__dict__.has_key('persistenceVersion')):
                continue
            currentVers = base.persistenceVersion
            pverName = '%s.persistenceVersion' % reflect.qual(base)
            persistVers = (self.__dict__.get(pverName) or 0)
            if persistVers:
                if not mergeCheck: 
                    del self.__dict__[pverName]
            assert persistVers <=  currentVers, x_("Either your idevices/generic.data file or the package you are loading was created with a newer version of eXe.  Please upgrade eXe and try again.")
            while persistVers < currentVers:
                persistVers = persistVers + 1
                method = base.__dict__.get('upgradeToVersion%s' % persistVers, None)
                if method:
                    if mergeCheck or isMerge: 
                        log.error("Old package with updates necessary:"
                                + " unable to insert.  " 
                                + " Please upgrade package first!") 
                        raise Exception(_(u"Package is old. Please upgrade it (using File..Open followed by File..Save As) before attempting to insert it into another package!"))
                    log.debug( "Upgrading " + reflect.qual(base) + " (of " \
                            +  reflect.qual(self.__class__) + " @ " \
                            +  str(id(self)) + ") to version " \
                            + str(persistVers) )
                    method(self)
                else:
                    if not mergeCheck: 
                        log.debug( 'no upgrade method for ' \
                               + reflect.qual(base)\
                               + ' to version ' + str(persistVers) )
            if not mergeCheck: 
                if base.__dict__.has_key("TwistedRePersist"): 
                    method = base.__dict__.get("TwistedRePersist") 
                    method(self)
