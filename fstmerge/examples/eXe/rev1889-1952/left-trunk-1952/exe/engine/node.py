"""
Nodes provide the structure to the package hierarchy
"""
import logging
import copy
from exe.engine.persist import Persistable
from exe.engine.path    import toUnicode
log = logging.getLogger(__name__)
class Node(Persistable):
    """
    Nodes provide the structure to the package hierarchy
    """
    persistenceVersion = 2
    def __init__(self, package, parent=None, title=""):
        """
        Initialize a new node
        """
        log.debug(u"init " + title)
        if parent:
            parent.children.append(self)
        self._package = package
        self._id      = package._regNewNode(self)
        self.parent   = parent
        self._title   = title
        self.children = []
        self.idevices = []
    def getId(self):
        """
        Returns our id.
        Used property to make it read only
        """
        return self._id
    id = property(getId)
    def getPackage(self):
        """
        Returns our package.
        Makes it read only
        """
        return self._package
    package = property(getPackage)
    def getLevel(self):
        """
        Calculates and returns our current level
        """
        return len(list(self.ancestors()))
    level = property(getLevel)
    def getTitle(self):
        """
        Returns our title as a string
        """
        if self._title:
            return toUnicode(self._title)
        else:
            return _(toUnicode(self.package.levelName(self.level - 1)))
    def setTitle(self, title):
        """
        Allows one to set the title as a string
        """
        if toUnicode(title) != toUnicode(self._title):
            self._title = title
            self.package.isChanged = True
    title = property(getTitle, setTitle)
    titleShort = property(lambda self: self.title.split('--', 1)[0].strip())
    titleLong = property(lambda self: self.title.split('--', 1)[-1].strip())
    def clone(self):
        """
        Clone a node just like this one, still belonging to this package
        """
        log.debug(u"clone ")
        return copy.deepcopy(self, {id(self._package): self._package})
    def ancestors(self):
        """Iterates over our ancestors"""
        if self.parent: # All top level nodes have no ancestors
            node = self
            while node is not self.package.root:
                node = node.parent
                yield node
    def isAncestorOf(self, node):
        """If we are an ancestor of 'node' returns 'true'"""
        return self in node.ancestors()
    def getResources(self):
        """
        Return the resource files used by this node
        """
        log.debug(u"getResources ")
        resources = {}
        for idevice in self.idevices:
            for resource in (idevice.systemResources + 
                             map(unicode, idevice.userResources)):
                resources[resource] = True
        return resources.keys()
    def createChild(self):
        """
        Create a child node
        """
        log.debug(u"createChild ")
        self.package.isChanged = True
        return Node(self.package, self)
    def delete(self):
        """
        Delete a node with all its children
        """
        log.debug(u"delete ")
        del self.package._nodeIdDict[self.id]
        if self.parent:
            self.parent.children.remove(self)
        while self.children:
            self.children[0].delete()
        while self.idevices:
            self.idevices[0].delete()
        self.package.isChanged = True
    def addIdevice(self, idevice):
        """
        Add the idevice to this node, sets idevice's parentNode 
        """
        log.debug(u"addIdevice ")
        idevice.id = self.package.getNewIdeviceId()
        idevice.parentNode = self
        for oldIdevice in self.idevices:
            oldIdevice.edit = False
        self.idevices.append(idevice)
    def move(self, newParent, nextSibling=None):
        """
        Moves the node around in the tree.
        nextSibling can be a node object or an integer index
        """
        log.debug(u"move ")
        if newParent:
            assert newParent.package is self.package, \
                   "Can't change a node into a different package"
        if self.parent:
            self.parent.children.remove(self)
        self.parent = newParent
        if newParent:
            children = newParent.children
            if nextSibling: 
                if type(nextSibling) is int:
                    children.insert(nextSibling, self)
                else:
                    children.insert(children.index(nextSibling), self)
            else:
                newParent.children.append(self)
        self.package.isChanged = True
    def mergeIntoPackage(self, package):
        """
        Changes the package of this node and all it's children
        """
        log.debug(u"mergeIntoPackage " + package.name)
        self._package = package
        self._id      = package._regNewNode(self)
        for idevice in self.idevices:
            resourceNamesChanged = []
            for resource in idevice.userResources:
                nameChanged = resource.changePackage(package)
                if nameChanged:
                    resourceNamesChanged.append(nameChanged)
            idevice.onResourceNamesChanged(resourceNamesChanged)
        for child in self.children:
            child.mergeIntoPackage(package)
    def promote(self):
        """
        Convenience function.
        Moves the node one step closer to the tree root.
        Returns True is successful
        """
        log.debug(u"promote ")
        if self.parent and self.parent.parent:
            self.move(self.parent.parent, self.parent.nextSibling())
            return True
        return False
    def demote(self):
        """
        Convenience function.
        Moves the node one step further away from its parent,
        tries to keep the same position in the tree.
        Returns True is successful
        """
        log.debug(u"demote ")
        if self.parent:
            idx = self.parent.children.index(self)
            if idx > 0:
                newParent = self.parent.children[idx - 1]
                self.move(newParent)
                return True
        return False
    def up(self):
        """
        Moves the node up one node vertically, keeping to the same level in 
        the tree.
        Returns True is successful.
        """
        log.debug(u"up ")
        if self.parent:
            children = self.parent.children
            i = children.index(self)
            if i > 0:
                children.remove(self)
                children.insert(i-1, self)
                self.package.isChanged = True
                return True
        return False
    def down(self):
        """
        Moves the node down one vertically, keeping its level the same.
        Returns True is successful.
        """
        log.debug(u"down ")
        if self.parent:
            children = self.parent.children
            i = children.index(self)
            children.remove(self)
            children.insert(i+1, self)
            self.package.isChanged = True
            return True
        return False
    def nextSibling(self):
        """Returns our next sibling or None"""
        log.debug(u"nextSibling ")
        sibling = None
        if self.parent:
            children = self.parent.children
            i = children.index(self) + 1
            if i < len(children):
                sibling = children[i]
        return sibling
    def previousSibling(self):
        """Returns our previous sibling or None"""
        log.debug(u"previousSibling ")
        sibling = None
        if self.parent:
            children = self.parent.children
            i = children.index(self) - 1
            if i > 0:
                sibling = children[i]
        return sibling
    def __str__(self):
        """
        Return a node as a string
        """
        nodeStr = ""
        nodeStr += self.id + u" "
        nodeStr += self.title + u"\n"
        for child in self.children:
            nodeStr += child.__str__()
        return nodeStr
    def upgradeToVersion1(self):
        """Upgrades the node from version 0 to 1."""
        log.debug(u"upgradeToVersion1 ")
        self._title = self.__dict__[u'title']
    def upgradeToVersion2(self):
        """Upgrades the node from eXe version 0.5."""
        log.debug(u"upgradeToVersion2 ")
        self._title = self._title.title
