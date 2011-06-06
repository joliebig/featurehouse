'''a module to handle groups'''
import abstract.GroupManager
class GroupManager(abstract.GroupManager.GroupManager):
    '''implementation of asbtract.GroupManager.GroupManager
    using emesenelib'''
    def __init__(self, dialog, protocol):
        '''initialize the object, dialog is a implementation
        of abstract.dialog, it's used to interact with the user'''
        abstract.GroupManager.GroupManager.__init__(self, 
            dialog, protocol)
        if protocol:
            self._connect_signals()
        else:
            debug('Protocol is None')
    def _connect_signals(self):
        '''connect the protocol signals'''
        self.protocol.connect('group-added', self.__on_group_added)
        self.protocol.connect('group-removed', self.__on_group_removed)
        self.protocol.connect('group-renamed', self.__on_group_renamed)
        self.protocol.connect('contact-added-to-group', 
            self.__on_contact_added_to_group)
        self.protocol.connect('contact-removed-from-group', 
            self.__on_contact_removed_from_group)
    def __on_group_added(self, protocol, name, identifier):
        '''method called when a new group is created'''
        self._on_group_added(name, identifier)
    def __on_group_removed(self, protocol, name):
        '''method called when a group is removed'''
        self._on_group_removed(name)
    def __on_group_renamed(self, protocol, old_name, new_name):
        '''method called when the name of the group is changed'''
        self._on_group_renamed(old_name, new_name)
    def __on_contact_added_to_group(self, protocol, account, group_name):
        '''callback called when an account is added to a group'''
        self._on_contact_added_to_group(account, group_name)
    def __on_contact_removed_from_group(self, protocol, account, group_name):
        '''callback called when an account is removed from a group'''
        self._on_contact_removed_from_group(account, group_name)
    def add(self, name):
        '''add a group'''
        if name:
            if self.protocol:
                self.protocol.addGroup(name)
        else:
            debug("invalid group name '%s'" % (name,))
    def rename(self, name, new_name):
        '''rename a group'''
        if new_name and name != new_name:
            if self.protocol:
                self.protocol.renameGroup(name, new_name)
        else:
            debug("invalid group name '%s'" % (new_name,))
    def remove(self, name):
        '''remove a group'''
        if self.protocol:
            self.protocol.removeGroup(name)
def debug(msg):
    '''debug method, the module send the debug here, it can be changed
    to use another debugging method'''
    return
    print('ContactManager.py: ', msg)
