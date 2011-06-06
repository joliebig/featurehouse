'''a module to handle contacts'''
import gettext
import stock
import status
import Object
import validators
_ = gettext.gettext
class ContactManager(Object.Object):
    '''this class represent an abstract class that provide methods
    to interact with contacts, implementing this class with the
    undeliying protocol library let the upper layer use the services
    of the protocol implementation independently of the api, in this
    way, the protocol library can be modified (or replaced) 
    without effect on the clients'''
    def __init__(self, dialog, protocol, account):
        '''initialize the object, dialog is a implementation
        ot dialog, it's used to interact with the user'''
        Object.Object.__init__(self)
        self.dialog = dialog
        self.protocol = protocol
        self.contacts = {}
        self.me = Contact(account)
        self.signal_add('contact-changed', 3)
        self.signal_add('contact-nick-changed', 2)
        self.signal_add('contact-alias-changed', 2)
        self.signal_add('contact-message-changed', 2)
        self.signal_add('contact-status-changed', 2)
        self.signal_add('contact-picture-changed', 2)
        self.signal_add('contact-media-changed', 2)
        self.signal_add('contact-blocked-changed', 2)
        self.signal_add('contact-added', 1)
        self.signal_add('contact-removed', 1)
    def register(self, contact):
        '''this method add the contact to the list of contacts'''
        if not self.exists(contact.account):
            self.contacts[contact.account] = contact
        else:
            debug("contact %s already in self.contacts" % (contact.account,))
    def exists(self, account):
        '''check if the account is on self.contacts, return True if exists'''
        if account in self.contacts:
            return True
        else:
            return False
    def _on_contact_added(self, account, identifier=None, nick=None, 
        message='', _status=status.OFFLINE, alias='', blocked=False):
        '''callback called when a new contact is added'''
        if account in self.contacts:
            debug("contact %s already on contacts" % (account,))
        else:
            if not nick:
                nick = account
            contact = Contact(account, identifier, nick, message, _status, 
                    alias, blocked)
            self.register(contact)
            self.signal_emit('contact-added', contact)
    def _on_contact_removed(self, account):
        '''callback called when a contact is removed'''
        if account in self.contacts:
            contact = self.contacts[account]
            del self.contacts[account]
            self.signal_emit('contact-removed', contact)
        else:
            debug("contact %s not in contacts" % (account,))
    def _on_contact_attr_changed(self, account, attr, value):
        '''callback called when an attribute is changed on a contact,
        this attribute can be block or alias (since these are the
        attributes that the user can change, the others are changed
        by the user itself)'''
        if not self.exists(account):
            debug("contact '%s' not in self.contacts" % (account,))
            return
        contact = self.contacts[account]
        if attr == 'block':
            old = contact.blocked
            contact._on_blocked_changed(value)    
            self.signal_emit('contact-blocked-changed', contact, old)
        elif attr == 'alias':
            old = contact.alias
            contact._on_alias_changed(value)    
            self.signal_emit('contact-alias-changed', contact, old)
        else:
            old = contact.attrs.get(attr, None)
        self.signal_emit('contact-changed', contact, attr, old)
    def _on_contact_nick_changed(self, account, nick):
        '''callback called when an user change his nick'''
        if not self.exists(account):
            debug("contact '%s' not in self.contacts" % (account,))
            return
        contact = self.contacts[account]
        old = contact.nick
        contact._on_nick_changed(nick)
        self.signal_emit('contact-changed', contact, 'nick', old)
        self.signal_emit('contact-nick-changed', contact, old)
    def _on_contact_message_changed(self, account, message):
        '''callback called when an user change his message'''
        if not self.exists(account):
            debug("contact '%s' not in self.contacts" % (account,))
            return
        contact = self.contacts[account]
        old = contact.message
        contact._on_message_changed(message)
        self.signal_emit('contact-changed', contact, 'message', old)
        self.signal_emit('contact-message-changed', contact, old)
    def _on_contact_status_changed(self, account, new_status):
        '''callback called when an user change his status'''
        if not self.exists(account):
            debug("contact '%s' not in self.contacts" % (account,))
            return
        if status.is_valid(new_status):
            contact = self.contacts[account]
            old = contact.status
            contact._on_status_changed(new_status)
            self.signal_emit('contact-changed', contact, 'status', old)
            self.signal_emit('contact-status-changed', contact, old)
        else:
            debug("invalid status '%s'" % (new_status,))
    def _on_contact_added_to_group(self, account, group_name):
        '''callback called when an account is added to a group'''
        if self.exists(account):
            if group_name not in self.contacts[account].groups:
                self.contacts[account].groups.append(group_name)
            else:
                debug("group '%s' already in account.groups" % (group_name,))
        else:
            debug("account %s not in self.contacts" % (account,))
    def _on_contact_removed_from_group(self, account, group_name):
        '''callback called when an account is removed from a group'''
        if self.exists(account):
            if group_name in self.contacts[account].groups:
                self.contacts[account].groups.remove(group_name)
            else:
                debug("group '%s' not in account.groups" % (group_name,))
        else:
            debug("account %s not in self.contacts" % (account,))
    def get_no_group(self):
        '''return a lost of contacts that dont belong to any group'''
        return [contact for contact in self.contacts.values() \
            if not contact.groups]
    def get_contacts(self, accounts):
        '''return a list of contact objects from a list of accounts'''
        return [self.contacts[account] for account in accounts if account \
            in self.contacts]
    def get_sorted_list_by_status(self, contacts=None):
        '''return a dict with status.* (OFFLINE, ONLINE etc) as key and
        a list of contact objects as value, you can use then 
        status.ORDERED to cycle over the keys.
        The contacts are sorted inside the status by display_name.
        if contacts is None, then use the internal list of contacts
        contacts should be a list of contact objects'''
        sorted_dict = {}
        contacts = contacts or self.contacts.values()
        for stat in status.ORDERED:
            sorted_dict[stat] = [contact for contact in contacts \
                if contact.status == stat]
            sorted_dict[stat].sort(cmp=lambda x, y: cmp(x.display_name, 
                y.display_name))
        return sorted_dict
    def get_sorted_list_by_group(self, groups, sort_by_status=False):
        '''return a dict with group names as keys and a list of sorted
        contacts as value, sort them according to display_name if
        sort_by_status is False, and by status and display_name if
        it's True'''
        groups.sort()
        sorted_dict = {}
        for group in groups:
            contacts = [contact for contact in self.contacts.values() \
                if group in contact.groups]
            if sort_by_status:
                sorted_dict[group] = self.get_sorted_list_by_status(contacts)
            else:
                contacts.sort(cmp=lambda x, y: cmp(x.display_name, 
                    y.display_name))
                sorted_dict[group] = contacts
        return sorted_dict
    def get_online_list(self, contacts=None):
        '''return a list of non offline contacts'''
        contacts = contacts or self.contacts.values()
        return [contact for contact in contacts \
                if contact.status != status.OFFLINE]
    def get_online_total_count(self, contacts):
        '''return a tuple with two values, the first is the number of
        non offline contacts on the list, the secont is the total number
        of contacts'''
        total = len(contacts)
        online = len([contact for contact in contacts \
            if contact.status != status.OFFLINE])
        return (online, total)
    def set_nick(self, nick):
        '''set the nick of our account to nick, dont forget to emit
        the contact-nick-changed and contact-changed signals with
        self.me as contact'''
        pass
    def get_nick(self, account=None):
        '''return the nick of the account, if account is None, return
        our nick'''
        if account:
            if self.exists(account):
                return self.contacts[account].nick
            else:
                debug("contact '%s' not in self.contacts" % (account,))
                return ''
        else:
            return self.me.nick
    def set_message(self, personal_message):
        '''set the personal message of our account to personal_message, 
        dont forget to emit
        the contact-message-changed and contact-changed signals with
        self.me as contact'''
        pass
    def get_message(self, account=None):
        '''return the personal message if account, an empty string if account
        doesn't exist, our message if account=None'''
        if account:
            if self.exists(account):
                return self.contacts[account].message
            else:
                debug("contact '%s' not in self.contacts" % (account,))
                return ''
        else:
            return self.me.message
    def set_media(self, media):
        '''set the current media on our account to media
        dont forget to emit
        the contact-media-changed and contact-changed signals with
        self.me as contact'''
        pass
    def get_media(self, account=None):
        '''return the media if account, an empty string if account
        doesn't exist, our media if account=None'''
        if account:
            if self.exists(account):
                return self.contacts[account].media
            else:
                debug("contact '%s' not in self.contacts" % (account,))
                return ''
        else:
            return self.me.media
    def set_status(self, new_status):
        '''set the status to status, the status should be one of the
        constants on status.py, consider calling status.is_valid.
        Also you should convert it to the values on the library
        dont forget to emit
        the contact-status-changed and contact-changed signals with
        self.me as contact'''
        pass
    def get_status(self, account=None):
        '''return the status of an account if exist, status.OFFLINE if dont
        if account == None, return the status of our user'''
        pass
    def set_picture(self, path):
        '''set the display picture to path'''
        pass
    def get_picture(self, account=None):
        '''return the picture of account, return the picture of the user
        if account is None, the type returned is a string with the content
        of the image'''
        if account:
            if self.exists(account):
                path = self.contacts[account].picture
            else:
                debug("contact '%s' not in self.contacts" % (account,))
                return None
        else:
            path = self.me.picture
        if validators.readable(path):
            return file(path, 'r').read()
        else:
            return None
    def set_alias(self, account, alias):
        '''set the contact alias, give an empty alias to reset
        dont forget to emit
        the contact-alias-changed and contact-changed signals'''
        pass
    def get_alias(self, account=None):
        '''return the alias of the account, if account is None, return
        our alias'''
        if account:
            if self.exists(account):
                return self.contacts[account].alias
            else:
                debug("contact '%s' not in self.contacts" % (account,))
                return ''
        else:
            return self.me.alias
    def get_display_name(self, account=None):
        '''return the alias or the nick, if account is None, return our
        alias or nick'''
        if account:
            if self.exists(account):
                return self.contacts[account].display_name
            else:
                debug("contact '%s' not in self.contacts" % (account,))
                return ''
        else:
            return self.me.display_name
    def block(self, account):
        '''block an user
        dont forget to emit
        the contact-blocked-changed and contact-changed signals'''
        pass
    def get_blocked(self, account):
        '''return True if blocked'''
        if self.exists(account):
            return self.contacts[account].blocked
        else:
            debug("contact '%s' not in self.contacts" % (account,))
            return False
    def unblock(self, account):
        '''unblock an user
        dont forget to emit
        the contact-blocked-changed and contact-changed signals'''
        pass
    def remove(self, account):
        '''remove an user'''
        pass
    def add(self, account, group=''):
        '''add an user'''
        pass
    def move_to_group(self, account, src_group, dest_group):
        '''move a user from src_group to dest_group'''
        pass
    def add_to_group(self, account, group):
        '''add an user to a group, return True on success'''
        pass
    def remove_from_group(self, account, group):
        '''remove an user from a group'''
        pass
    def set_nick_dialog(self):
        '''show a dialog asking to change the nick'''
        self.dialog.set_nick(self.get_nick(), self.set_nick_cb)
    def set_message_dialog(self, old_personal_message):
        '''show a dialog asking to change the personal message'''
        self.dialog.set_message(old_personal_message, 
            self.set_message_cb)
    def set_alias_dialog(self, account):
        '''show the alias dialog'''
        self.dialog.set_contact_alias(account, self.get_alias(account), 
            self.set_alias_cb)
    def set_picture_dialog(self, picture_path, picture_dir):
        '''show a dialog to select a new picture, picture_path
        is the actual picture image path and picture_dir is a
        path to a dir where all the pictures to select are located'''
        self.dialog.set_picture(picture_path, picture_dir,
            self._set_picture_cb)
    def remove_dialog(self, account):
        '''show a confirmation dialog to ask if sure to remove the
        user, it's optional to use, but recomended'''
        self.dialog.yes_no(
            _("Are you sure you want to delete %s?") % (account, ),
            self.remove_cb, account)
    def add_dialog(self):
        '''show a dialog to ask for the account, and if the account
        is valid, add the user'''
        self.dialog.add_contact(self.add_cb)
    def set_nick_cb(self, response, old_nick='', new_nick=''):
        '''callback for the dialog.set_nick method'''
        if response == stock.ACCEPT:
            if old_nick == new_nick:
                debug('old nick and new nick are the same')
                return
            elif new_nick == '':
                debug('empty new nick')
                return
            self.set_nick(new_nick)
    def set_message_cb(self, response, old_pm='', new_pm=''):
        '''callback for the dialog.set_message method'''
        if response == stock.ACCEPT:
            if old_pm == new_pm:
                debug('old and new personal messages are the same')
                return
            self.set_message(new_pm)
    def set_alias_cb(self, response, account='', old_alias='', new_alias=''):
        '''callback for the dialog.set_contact_alias method,
        the parameters and the values are described on that method'''
        if response == stock.ACCEPT:
            if old_alias == new_alias:
                debug('old alias and new alias are the same')
                return
            self.set_alias(account, new_alias)
        elif response == stock.CLEAR:
            self.set_alias(account, '')
    def remove_cb(self, response, account=''):
        '''callback for dialog.yes_no, asking to confirm the 
        user remove'''
        if response == stock.YES:
            self.remove(account)
    def add_cb(self, response, account='', groups=''):
        '''callback to the add_dialog method, add the user and add him 
        to the defined groups'''
        if response == stock.ADD:
            self.add(account)
            if groups:
                for group in groups:
                    self.add_to_group(account, group)
    def _set_picture_cb(self, response, path):
        '''callback to the set_picture_dialog, change the picture of
        self.me'''
        if response == stock.ACCEPT:
            self.set_picture(path)
        elif response == stock.CLEAR:
            self.set_picture('')
class Contact(object):
    '''a class that represent a contact'''
    def __init__(self, account, identifier=None, nick='', message=None,
        _status=status.OFFLINE, alias='', blocked=False):
        '''class contructor'''
        self.account = account
        self.identifier = identifier or '0'
        self.nick = nick or self.account
        self.message = message or ''
        self.media = ''
        self.status = _status
        self.alias = alias
        self.blocked = blocked
        self.picture = None
        self.groups = []
        self.attrs = {}
    def _on_nick_changed(self, nick):
        '''callback called on nick change'''
        self.nick = nick
    def _on_message_changed(self, message):
        '''callback called on message change'''
        self.message = message
    def _on_status_changed(self, _status):
        '''callback called on status change'''
        self.status = _status
    def _on_alias_changed(self, alias):
        '''callback called on alias change'''
        self.alias = alias
    def _on_blocked_changed(self, blocked):
        '''callback called on blocked change'''
        self.blocked = blocked
    def _get_display_name(self):
        '''return the alias if exist, if not the nick if not empty, if not
        the mail'''
        return self.alias or self.nick or self.account
    display_name = property(fget=_get_display_name)
    def _get_status_string(self):
        '''return a string representation of the status'''
        return status.STATUS.get(self.status, '?')
    status_string = property(fget=_get_status_string)
    def __repr__(self):
        '''return a string representation of the object'''
        return "<contact account='%s'>" % (self.account,)
def debug(msg):
    '''debug method, the module send the debug here, it can be changed
    to use another debugging method'''
    return
    print('ContactManager.py: ', msg)
