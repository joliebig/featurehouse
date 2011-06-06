"""
Nodes provide the structure to the package hierarchy
"""
import logging
from copy               import deepcopy
from exe.engine.persist import Persistable
from exe.engine.path    import toUnicode
from exe                import globals as G
from urllib             import quote
from exe.webui                import common
log = logging.getLogger(__name__)
class Node(Persistable):
    """
    Nodes provide the structure to the package hierarchy
    """
    persistenceVersion = 2
    nonpersistant      = ['tmp_export_filename']
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
        self.last_full_node_path = self.GetFullNodePath()
    def getId(self):
        """
        Returns our id.
        Used property to make it read only
        """
        if hasattr(self, '_id'): 
            return self._id
        else:
            return None
    id = property(getId)
    def getPackage(self):
        """
        Returns our package.
        Makes it read only
        """
        if hasattr(self, '_package'): 
            return self._package
        else:
            return None
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
        if hasattr(self, '_title') and self._title:
            return toUnicode(self._title)
        elif hasattr(self, '_package') and self.package is not None:
            return _(toUnicode(self.package.levelName(self.level - 1)))
        else:
            return u'Unknown Node [no title or package]'
    def TwistedRePersist(self):
        """
        Handles any post-upgrade issues 
        (such as typically re-persisting non-persistent data)
        that slipped between the cracks....
        """
        self.last_full_node_path = self.GetFullNodePath()
    def GetFullNodePath(self, new_node_title=""):
        """
        A general purpose single-line node-naming convention,
        currently only used for the anchor names, to
        provide a path to its specific node.
        Create this path in an HTML-safe name, to closely match 
        the names used upon export of the corresponding files.
        Optional new_node_title allows the determination of the
        full path name should this node's name change.
        """
        full_path = "exe-node"
        this_nodes_ancestors = list(self.ancestors())
        num_ancestors = len(this_nodes_ancestors)
        for loop in range(num_ancestors-1, -1, -1):
            node = this_nodes_ancestors[loop]
            if node is not None:
                full_path = full_path + ":" \
                        + quote(node.getTitle().encode('utf8'))
        if new_node_title == "":
            full_path = full_path + ":" + quote(self.getTitle().encode('utf8'))
        else:
            full_path = full_path + ":" + quote(new_node_title.encode('utf8'))
        return full_path
    def RenamedNodePath(self, isMerge=False, isExtract=False):
        """
        To update all of the anchors (if any) that are defined within
        any of this node's various iDevice fields, and any 
        internal links corresponding to those anchors.
        Called AFTER the actual rename has occurred.
        NOTE: isMerge & isExtract will also attempt to connect all the data 
        structures, and isExtract will also try to clear out any orphaned links.
        AND: especially for extracts, continue on through all the child nodes
        even if the node names & path appear to be the same, since the objects
        actually differ and internal linking data structures need to be updated.
        """
        if not hasattr(self, 'anchor_fields'):
            self.anchor_fields = []
        old_node_path = self.last_full_node_path
        new_node_path = self.GetFullNodePath()
        self.last_full_node_path = new_node_path
        log.debug('Renaming node path, from "' + old_node_path 
                + '" to "' + new_node_path + '"')
        current_package = self.package
        for this_field in self.anchor_fields:
            if (isMerge or isExtract) and hasattr(this_field, 'anchor_names') \
            and len(this_field.anchor_names) > 0:
                if not hasattr(self.package, 'anchor_fields'):
                    self.package.anchor_fields = []
                if this_field not in self.package.anchor_fields:
                    self.package.anchor_fields.append(this_field)
                if not hasattr(self.package, 'anchor_nodes'):
                    self.package.anchor_nodes = []
                if self not in self.package.anchor_nodes:
                    self.package.anchor_nodes.append(self)
            if hasattr(this_field, 'anchor_names') \
            and hasattr(this_field, 'anchors_linked_from_fields'):
                for this_anchor_name in this_field.anchor_names:
                    old_full_link_name = old_node_path + "#" + this_anchor_name
                    new_full_link_name = new_node_path + "#" + this_anchor_name
                    num_links = len(this_field.anchors_linked_from_fields[\
                            this_anchor_name])
                    for i in range(num_links-1, -1, -1):
                        that_field = this_field.anchors_linked_from_fields[\
                            this_anchor_name][i]
                        that_field_is_valid = True
                        if isExtract: 
                            if that_field.idevice is None \
                            or that_field.idevice.parentNode is None \
                            or that_field.idevice.parentNode.package \
                            != current_package \
                            or that_field.idevice.parentNode.id \
                            not in current_package._nodeIdDict \
                            or current_package._nodeIdDict[ \
                            that_field.idevice.parentNode.id] \
                            != that_field.idevice.parentNode:
                                that_field_is_valid = False
                                this_field.anchors_linked_from_fields[\
                                        this_anchor_name].remove(that_field)
                        if that_field_is_valid: 
                            that_field.RenameInternalLinkToAnchor(\
                                this_field, unicode(old_full_link_name), 
                                unicode(new_full_link_name))
        this_anchor_name = u"auto_top"
        old_full_link_name = old_node_path + "#" + this_anchor_name
        new_full_link_name = new_node_path + "#" + this_anchor_name
        if not hasattr(self, 'top_anchors_linked_from_fields'):
            self.top_anchors_linked_from_fields = []
        num_links = len(self.top_anchors_linked_from_fields)
        num_top_links = num_links
        if (isMerge or isExtract):
            if not hasattr(self.package, 'anchor_nodes'):
                self.package.anchor_nodes = []
            if num_links > 0 and self not in self.package.anchor_nodes:
                self.package.anchor_nodes.append(self)
        for i in range(num_links-1, -1, -1):
            that_field = self.top_anchors_linked_from_fields[i]
            that_field_is_valid = True
            if isExtract: 
                if that_field.idevice is None \
                or that_field.idevice.parentNode is None \
                or that_field.idevice.parentNode.package \
                != current_package \
                or that_field.idevice.parentNode.id \
                not in current_package._nodeIdDict \
                or current_package._nodeIdDict[ \
                that_field.idevice.parentNode.id] \
                != that_field.idevice.parentNode:
                    that_field_is_valid = False
                    self.top_anchors_linked_from_fields.remove(that_field)
            if that_field_is_valid: 
                anchor_field = self
                that_field.RenameInternalLinkToAnchor(\
                    anchor_field, unicode(old_full_link_name), 
                    unicode(new_full_link_name))
        num_links = len(self.top_anchors_linked_from_fields)
        if num_top_links > 0 and num_links <= 0:
            if len(self.anchor_fields) <= 0:
                if self.package and hasattr(self.package, 'anchor_nodes') \
                and self in self.package.anchor_nodes:
                    self.package.anchor_nodes.remove(self)
        if isExtract:
            for this_idevice in self.idevices:
                for this_field in this_idevice.getRichTextFields(): 
                    if hasattr(this_field, 'intlinks_to_anchors') \
                    and len(this_field.intlinks_to_anchors) > 0: 
                        these_link_names = this_field.intlinks_to_anchors.keys()
                        num_links = len(these_link_names)
                        for i in range(num_links-1, -1, -1):
                            this_link_name = these_link_names[i]
                            this_anchor_field = \
                                this_field.intlinks_to_anchors[this_link_name] 
                            from exe.engine.field         import Field
                            this_link_node = None
                            this_anchor_name = common.getAnchorNameFromLinkName(
                                    this_link_name)
                            if this_anchor_field \
                            and isinstance(this_anchor_field, Field) \
                            and this_anchor_name != u"auto_top": 
                                if this_anchor_field.idevice is not None \
                                and this_anchor_field.idevice.parentNode:
                                    this_link_node = \
                                        this_anchor_field.idevice.parentNode
                            elif this_anchor_field \
                            and isinstance(this_anchor_field, Node):
                                this_link_node = this_anchor_field
                            if this_link_node is None \
                            or this_link_node.package != current_package \
                            or this_link_node.id \
                            not in current_package._nodeIdDict \
                            or current_package._nodeIdDict[this_link_node.id] \
                            != this_link_node:
                                this_field.RemoveInternalLinkToRemovedAnchor( \
                                    this_anchor_field, unicode(this_link_name))
        for child_node in self.children:
            child_node.RenamedNodePath(isMerge, isExtract)
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
    def copyToPackage(self, newPackage, newParentNode=None):
        """
        Clone a node just like this one, still belonging to this package.
        if 'newParentNode' is None, the newly created node will replace the 
            root of 'newPackage'
        The newly inserted node is automatically selected.
        """
        log.debug(u"clone " + self.title)
        G.application.persistNonPersistants = True
        try: 
            newNode = deepcopy(self, {id(self._package): newPackage,
                                  id(self.parent): None}) 
            newNode._id = newPackage._regNewNode(newNode)
        except Exception, e:
            G.application.persistNonPersistants = False
            raise
        G.application.persistNonPersistants = False
        for node in newNode.walkDescendants():
            node._id = newPackage._regNewNode(node)
        if newParentNode is None:
            newNode.parent = None
            newPackage.root = newPackage.currentNode = newNode
        else:
            newNode.parent = newParentNode
            newNode.parent.children.append(newNode)
            newPackage.currentNode = newNode
        return newNode
    def ancestors(self):
        """Iterates over our ancestors"""
        if self.parent: # All top level nodes have no ancestors
            node = self
            while node is not None and node is not self.package.root:
                if not hasattr(node, 'parent'):
                    log.warn("ancestor node has no parent")
                    node = None
                else: 
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
            reses = [toUnicode(res.storageName, 'utf8') for res in idevice.userResources]
            for resource in (idevice.systemResources + reses):
                resources[resource] = True
        return resources.keys()
    def createChild(self):
        """
        Create a child node
        """
        log.debug(u"createChild ")
        self.package.isChanged = True
        return Node(self.package, self)
    def delete(self, pruningZombies=False):
        """
        Delete a node with all its children
        """
        delete_msg = ""
        if pruningZombies:
            delete_msg += "pruning zombie Node "
        else:
            delete_msg += "deleting Node "
        delete_msg += "[parent="
        if hasattr(self, 'parent') and self.parent:
            delete_msg += "\"" + self.parent._title + "\"] \""
        else:
            delete_msg += "None] \""
        delete_msg += self.getTitle() + "\" nodeId=" + str(self.getId()) \
            + ", @ \"" + str(id(self)) +"\""
        if pruningZombies: 
            log.warn(delete_msg)
        else:
            log.debug(delete_msg)
        this_node_path = self.GetFullNodePath()
        this_anchor_name = u"auto_top"
        full_link_name = this_node_path + "#" + this_anchor_name
        if not hasattr(self, 'top_anchors_linked_from_fields'):
            self.top_anchors_linked_from_fields = []
        for this_field in self.top_anchors_linked_from_fields: 
            this_anchor_field = self
            this_field.RemoveInternalLinkToRemovedAnchor( \
                    this_anchor_field, full_link_name)
        if hasattr(self, '_package') and self.package is not None \
        and self.id in self.package._nodeIdDict \
        and self.package._nodeIdDict[self.id] == self: 
            if pruningZombies:
                if self.package and self.package.root == self:
                    if self.parent and self in self.parent.children: 
                        self.parent.children.remove(self)
                    self.parent = None
                    log.warn("While pruning zombie nodes, found ROOT node \"" 
                        + self._title + "\" still in package node dictionary. "
                        + "Stopping the prune on this part of the node tree.")
                    self.package.isChanged = True
                    return
                elif self.parent:
                    if self in self.parent.children: 
                        self.parent.children.remove(self)
                    self.parent = None
                    log.warn("While pruning zombie nodes, found node \"" 
                        + self._title + "\" still in package node dictionary. "
                        + "Stopping the prune on this part of the node tree.")
                    self.package.isChanged = True
                    return
                del self.package._nodeIdDict[self.id]
            else: 
                del self.package._nodeIdDict[self.id]
        if hasattr(self, 'parent') and self.parent:
            if hasattr(self.parent, 'children')\
            and self in self.parent.children: 
                self.parent.children.remove(self)
            self.parent = None
        num_children = 0
        if hasattr(self, 'children'):
            num_children = len(self.children)
        for i in range(num_children-1, -1, -1):
            if self.children[i]:
                if self.children[i].parent is None:
                    log.warn('reconnecting child node before deletion from node')
                    self.children[i].parent = self
                elif self.children[i].parent != self:
                    log.warn('about to delete child node from node, '\
                            'but it points to a different parentNode.')
                self.children[i].delete(pruningZombies)
        num_idevices = 0
        if hasattr(self, 'idevices'): 
            num_idevices = len(self.idevices)
        for i in range(num_idevices-1, -1, -1):
            if self.idevices[i].parentNode is None:
                log.warn('reconnecting iDevice before deletion from node')
                self.idevices[i].parentNode = self
            elif self.idevices[i].parentNode != self:
                log.warn('about to delete iDevice from node, '\
                        'but it points to a different parentNode.')
            self.idevices[0].delete()
        if self.package: 
            self.package.isChanged = True
            self._package = None
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
        self.RenamedNodePath()
        self.package.isChanged = True
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
    def walkDescendants(self):
        """
        Generator that walks all descendant nodes
        """
        for child in self.children:
            yield child
            for descendant in child.walkDescendants():
                yield descendant
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
    def launch_testForZombies(self):
        """
        a wrapper to testForZombieNodes(self), such that it might be called
        after the package has been loaded and upgraded.  Otherwise, due 
        to the seemingly random upgrading of the package and resource objects,
        this might be called too early.
        """
        if not hasattr(self, 'parent') or self.parent is None: 
            G.application.afterUpgradeHandlers.append(self.testForZombieNodes)
        elif not hasattr(self.parent, 'children')\
        or not self in self.parent.children: 
            G.application.afterUpgradeHandlers.append(self.testForZombieNodes)
    def testForZombieNodes(self):
        """ 
        testing a possible post-load confirmation that this resource 
        is indeed attached to something.  
        to be called from twisted/persist/styles.py upon load of a Node.
        """
        if not hasattr(self, '_package') or self._package is None\
        or not hasattr(self._package, 'root') or self._package.root != self: 
            log.warn("Found zombie Node \"" + self.getTitle() 
                + "\", nodeId=" + str(self.getId()) 
                + " @ " + str(id(self)) + ".")
            if not hasattr(self, '_title'):
                self._title = self.getTitle()
            zombie_preface = u"ZOMBIE("
            if self._title[0:len(zombie_preface)] != zombie_preface: 
                self._title = zombie_preface + self._title + ")"
            G.application.afterUpgradeZombies2Delete.append(self)
