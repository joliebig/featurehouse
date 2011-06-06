'''a module to handle groups'''
import gettext
import stock
import Object
_ = gettext.gettext
class GroupManager(Object.Object):
    '''this class represent an abstract class that provide methods
    to interact with groups, implementing this class with the
    undeliying protocol library let the upper layer use the services
    of the protocol implementation independently of the api, in this
    way, the protocol library can be modified (or replaced) 
    without effect on the clients'''
    def __init__(self, dialog, protocol):
        '''initialize the object, dialog is a implementation
        of abstract.dialog, it's used to interact with the user'''
        Object.Object.__init__(self)
        self.dialog = dialog
        self.protocol = protocol
        self.groups = {}
        self.signal_add('group-changed', 3)
        self.signal_add('group-name-changed', 2)
        self.signal_add('group-added', 1)
        self.signal_add('group-removed', 1)
        self.signal_add('contact-added-to-group', 2)
        self.signal_add('contact-removed-from-group', 2)
    def register(self, group):
        '''add a group object to the list'''
        if not self.exists(group.name):
            self.groups[group.name] = group
        else:
            debug("group %s already in groups" % (group.name,))
    def exists(self, group_name):
        '''check if the group is on self.groups, return True if exists'''
        if group_name in self.groups:
            return True
        else:
            return False
    def _on_group_added(self, name, identifier=None, contacts=None):
        '''method called when a new group is created'''
        if self.exists(name):
            debug("group %s already in groups" % (name,))
        else:
            group = Group(name, identifier, contacts)
            self.groups[name] = group 
            self.signal_emit('group-added', group)
    def _on_group_removed(self, name):
        '''method called when a group is removed'''
        if self.exists(name):
            group = self.groups[name]
            del self.groups[name]
            self.signal_emit('group-removed', group)
        else:
            debug("group %s not in groups" % (name,))
    def _on_group_renamed(self, old_name, new_name):
        '''method called when the name of the group is changed'''
        if self.exists(old_name):
            group = self.groups[old_name]
            group.name = new_name
            self.groups[group.name] = group
            self.signal_emit('group-name-changed', group, old_name)
            self.signal_emit('group-changed', group, 'name', old_name)
        else:
            debug("group %s not in groups" % (old_name,))
    def _on_contact_added_to_group(self, account, group_name):
        '''callback called when an account is added to a group'''
        if self.exists(group_name):
            if account not in self.groups[group_name].contacts:
                self.groups[group_name].contacts.append(account)
                self.signal_emit('contact-added-to-group', 
                    account, self.groups[group_name])
            else:
                debug("account %s already in group %s" % (account, 
                    group_name))
        else:
            debug("group %s not in self.groups" % (group_name,))
    def _on_contact_removed_from_group(self, account, group_name):
        '''callback called when an account is removed from a group'''
        if self.exists(group_name):
            if account in self.groups[group_name].contacts:
                self.groups[group_name].contacts.remove(account)
                self.signal_emit('contact-removed-from-group', 
                    account, self.groups[group_name])
            else:
                debug("account %s not in group %s" % (account, 
                    group_name))
        else:
            debug("group %s not in self.groups" % (group_name,))
    def add(self, name):
        '''add a group'''
        pass
    def rename(self, name, new_name):
        '''rename a group'''
        pass
    def remove(self, name):
        '''remove a group'''
        pass
    def add_dialog(self):
        '''show a dialog to add a group'''
        self.dialog.add_group(self.add_cb)
    def rename_dialog(self, old_name):
        '''show a dialog showing the actual name of a group
        and asking for the new one'''
        self.dialog.rename_group(old_name, self.rename_cb)
    def remove_dialog(self, name):
        '''ask for confirmation on group deletion, it can be used the method
        directly, but it's good to ask :P'''
        self.dialog.yes_no(_(
            _("Are you sure you want to delete the %s group?") % (name, )),
            self.remove_cb, name)
    def add_cb(self, response, group_name=''):
        '''callback for the dialog.add_group method'''
        if response == stock.ACCEPT:
            if group_name:
                self.add(group_name)
    def rename_cb(self, response, old_name='', new_name=''):
        '''callback called by dialog.rename_group'''
        if response == stock.ACCEPT:
            if old_name == new_name:
                self.dialog.warning(_("Old and new name are the same"))
            elif new_name:
                self.rename(old_name, new_name)
            else:
                self.dialog.warning(_("new name not valid"))
    def remove_cb(self, response, group_name=''):
        '''callback for the dialog.yes_no method, asking for
        confirmation un group delete'''
        if response == stock.YES:
            self.remove(group_name)
class Group(object):
    '''a class representing a group'''
    def __init__(self, name, identifier=None, contacts=None):
        '''class constructor'''
        self.name = name
        self.identifier = identifier or '0'
        self.contacts = contacts or []
    def _on_contact_added(self, account):
        '''callback called when a contact is added to this group'''
        self.contacts.append(account)
    def _on_contact_removed(self, account):
        '''callback called when a contact is removed from this group'''
        if account in self.contacts:
            del self.contacts[account]
    def __repr__(self):
        '''return a string representation of the object'''
        return "<group name='%s'>" % (self.name,)
def debug(msg):
    '''debug method, the module send the debug here, it can be changed
    to use another debugging method'''
    return
    print('GroupManager.py: ', msg)
