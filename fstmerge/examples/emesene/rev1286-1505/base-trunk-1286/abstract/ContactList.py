'''a abstract object that define the API of a contact list and some behavior'''
import status
import Object
class ContactList(Object.Object):
    '''an abstract class that defines the api that the contact list should
    have'''
    def __init__(self, contacts, groups, dialog):
        '''class constructor'''
        Object.Object.__init__(self)
        self.signal_add('contact-selected', 1)
        self.signal_add('group-selected', 1)
        self.signal_add('group-menu-selected', 1)
        self.signal_add('contact-menu-selected', 1)
        self.contacts = contacts
        self.groups = groups
        self.dialog = dialog
        self.group_state = {}
        self.order_by_group = True
        self.show_nick = True
        self.show_empty_groups = False
        self.show_group_count = False
        self._show_offline = False
        self._filter_text = ''
        self.nick_template = \
            '%DISPLAY_NAME%\n%ACCOUNT%\n(%STATUS%) - %MESSAGE%'
        self.group_template = '%NAME% (%ONLINE_COUNT%/%TOTAL_COUNT%)'
        self._connect_signals()
    def _connect_signals(self):
        '''connect all the signals from ContactManager and GroupManager'''
        self.contacts.signal_connect('contact-added', self._on_contact_added)
        self.contacts.signal_connect('contact-removed', 
            self._on_contact_removed)
        self.contacts.signal_connect('contact-changed', 
            self._on_contact_changed)
        self.groups.signal_connect('group-added', self._on_group_added)
        self.groups.signal_connect('group-removed', self._on_group_removed)
        self.groups.signal_connect('group-changed', self._on_group_changed)
        self.groups.signal_connect('contact-added-to-group', 
            self._on_contact_added_to_group)
        self.groups.signal_connect('contact-removed-from-group', 
            self._on_contact_removed_from_group)
    def _on_contact_added(self, contacts, contact):
        '''callback called when a contact is added to the userlist'''
        self.add_contact(contact)
    def _on_contact_removed(self, contacts, contact):
        '''callback called when a contact is removed from the userlist'''
        self.remove_contact(contact)
    def _on_contact_changed(self, contacts, contact, attr, old_value):
        '''callback called when an attr changes on a contact'''
        self.update_contact(contact)
    def _on_group_added(self, groups, group):
        '''called when a group is added'''
        self.add_group(group)
    def _on_group_removed(self, groups, group):
        '''called when a group'''
        self.remove_group(group)
    def _on_group_changed(self, groups, group, attr, old_value):
        '''called when an attr from a group changed'''
        self.update_group(group)
    def _on_contact_added_to_group(self, groups, account, group):
        '''callback called when a contact is added to the a group'''
        contact = self.contacts.contacts.get(account, None)
        if contact:
            self.add_contact(contact, group)
        else:
            print account, 'has no contact associated'
    def _on_contact_removed_from_group(self, groups, account, group):
        '''callback called when a contact is removed from the a group'''
        contact = self.contacts.contacts.get(account, None)
        if contact:
            self.remove_contact(contact, group)
        else:
            print account, 'has no contact associated'
    def _get_order_by_status(self):
        '''return the value of order by status'''
        return not self.order_by_group
    def _set_order_by_status(self, value):
        '''set the value of order by status'''
        self.order_by_group = not value
    order_by_status = property(fget=_get_order_by_status, 
        fset=_set_order_by_status)
    def _get_show_offline(self):
        '''return the value of self._show_offline'''
        return self._show_offline
    def _set_show_offline(self, value):
        '''set the value of self._show_offline to value and call to 
        self.refilter()'''
        self._show_offline = value
        self.refilter()
    show_offline = property(fget=_get_show_offline, fset=_set_show_offline)
    def _get_filter_text(self):
        '''return the filter_text value'''
        return self._filter_text
    def _set_filter_text(self, value):
        '''set the filter_text value'''
        self._filter_text = value
        self.refilter()
    filter_text = property(fget=_get_filter_text, fset=_set_filter_text)
    def format_nick(self, contact):
        '''replace the appearance of the template vars using the values of
        the contact
        '''
        template = self.nick_template
        template = template.replace('%NICK%', contact.nick)
        template = template.replace('%ACCOUNT%', contact.account)
        template = template.replace('%MESSAGE%', contact.message)
        template = template.replace('%STATUS%', status.STATUS[contact.status])
        template = template.replace('%DISPLAY_NAME%', contact.display_name)
        return template
    def format_group(self, group):
        '''replace the appearance of the template vars using the values of
        the group
        '''
        contacts = self.contacts.get_contacts(group.contacts)
        (online, total) = self.contacts.get_online_total_count(contacts)       
        template = self.group_template
        template = template.replace('%NAME%', group.name)
        template = template.replace('%ONLINE_COUNT%', str(online))
        template = template.replace('%TOTAL_COUNT%', str(total))
        return template
    def refilter(self):
        '''refilter the values according to the value of self.filter_text'''
        raise NotImplementedError
    def is_group_selected(self):
        '''return True if a group is selected'''
        raise NotImplementedError
    def is_contact_selected(self):
        '''return True if a contact is selected'''
        raise NotImplementedError
    def get_group_selected(self):
        '''return a group object if there is a group selected, None otherwise
        '''
        raise NotImplementedError
    def get_contact_selected(self):
        '''return a contact object if there is a group selected, None otherwise
        '''
        raise NotImplementedError
    def add_group(self, group):
        '''add a group to the contact list'''
        raise NotImplementedError
    def remove_group(self, group):
        '''remove a group from the contact list'''
        raise NotImplementedError
    def add_contact(self, contact, group=None):
        '''add a contact to the contact list, add it to the group if 
        group is not None'''
        raise NotImplementedError
    def remove_contact(self, contact, group=None):
        '''remove a contact from the specified group, if group is None
        then remove him from all groups'''
        raise NotImplementedError
    def fill(self):
        '''fill the contact list with the contacts and groups from
        self.contacts and self.groups'''
        for group in self.groups.groups:
            contacts = self.contacts.get_contacts(group.contacts)
            for contact in contacts:
                self.add_contact(contact, group)
        for contact in self.contacts.get_no_group():
            self.add_contact(contact)
    def clear(self):
        '''clear the contact list'''
        raise NotImplementedError
    def update_contact(self, contact):
        '''update the data of contact'''
        raise NotImplementedError
    def update_group(self, group):
        '''update the data of group'''
        raise NotImplementedError
    def set_group_state(self, group, state):
        '''expand group id state is True, collapse it if False'''
        raise NotImplementedError
    def expand_collapse_groups(self):
        '''expand and collapse the groups according to the state of the
        group'''
        for (group, state) in self.group_state.iteritems():
            self.set_group_state(group, state)
    def on_group_collapsed(self, group):
        '''called when a group is collapsed, update the status of the
        groups'''
        self.group_state.update({group.name:False})
    def on_group_expanded(self, group):
        '''called when a group is expanded, update the status of the
        groups'''
        self.group_state.update({group.name:True})
    def compare_groups(self, group1, group2):
        '''compare two groups and return 1 if group1 should go first, 0
        if equal, -1 if group2 should go first'''
        return cmp(group1.name, group2.name)
    def compare_contacts(self, contact1, contact2):
        '''compare two contacts and return 1 if contact1 should go first, 0
        if equal and -1 if contact2 should go first'''
        result = cmp(status.ORDERED.index(contact1.status), 
            status.ORDERED.index(contact2.status))
        if result != 0:
            return result
        if self.order_by_status:
            return cmp(contact1.display_name, contact2.display_name)
        if len(contact1.groups) == 0:
            if len(contact2.groups) == 0:
                return cmp(contact1.display_name, contact2.display_name)
            else:
                return -1
        elif len(contact2.groups) == 0:
            return 1
        else:
            return cmp(contact1.display_name, contact2.display_name)
