import common
import os
import sha
class Contact(object):
    '''class describing a contact'''
    def __init__(self, email, id='', nick='', personalMessage='', alias='', \
         status='FLN', mobile=False, blocked=False, space=False, allow=False, \
         reverse=False, pending=False, groups=None, dummy=False, clientid=0):
        '''Constructor'''
        self.email = email.lower()
        self.id = id
        self.nick = nick
        self.personalMessage = personalMessage
        self.alias = alias
        self.status = status
        self.mobile = mobile
        self.space = space
        self.dummy = dummy
        self.clientid = clientid
        if groups:
            self.groups = groups # for performance
        else:
            self.groups = []
        self.allow = allow
        self.blocked = blocked
        self.reverse = reverse
        self.pending = pending
        self.locked = False
        self.msnobj = None
        self._dpPath = ''
        self.cid = 0
    def __repr__(self):
        return self.email + ': ' + self.id + ' ' + self.nick + '\n'\
                         + str(self.groups)
    def _getEmail(self):
        return self._email
    def _setEmail(self, value):
        self._email = value.lower()
    email = property(_getEmail, _setEmail, None)
    def _getPath(self):
        if self._dpPath:
            return self._dpPath
        elif self.msnobj is None:
            return ''
        sha1d = sha.sha(self.msnobj.sha1d).hexdigest()
        return self.email.split('@')[0] + "_" + sha1d
    def _setPath(self, value):
        self._dpPath = value
    displayPicturePath = property(_getPath, _setPath, None)
    def addGroup(self, id):
        if not id in self.groups:
            self.groups.append(id)
    def removeGroup(self, id):
        id = str(id)
        if id in self.groups:
            self.groups.remove(id)
        else:
            common.debug('Group %s not found' % id)
    def shares_webcam(self):
        return ((self.clientid & 0x10) == 0x10)
    def getMSNC(self):
        return ((self.clientid & 0xf0000000) >> 28)
class Group(object):
    '''class representing a group'''
    def __init__(self, name, id = ''):
        '''Contructor,
        users is a dict with an email as key and a contact object as value'''
        self.name = name
        self.id = id
        self.users = {}
    def getUsersByStatus(self, status):
        '''Returns a list user users according to its status'''
        return [i for i in self.users.values() if \
             i.status == common.status_table[status]]
    def getUser(self, email):
        email = email.lower()
        if self.users.has_key(email):
            return self.users[email]
        else:
            None
    def setUser(self, email, contactObject):
        email = email.lower()
        self.users[email] = contactObject
    def removeUser(self, email):
        email = email.lower()
        del self.users[email]
    def getSize(self):
        '''returns how many users the group has'''
        return len(self.users)
    def getOnlineUsersNumber(self):
        '''Returns the number of online users (not offline) in the group'''
        offline = self.getUsersByStatus('offline')
        return self.getSize() - len(offline)
class ContactList(object):
    '''a class that contains groups that contains users'''
    def __init__(self, groups=None):
        '''Constructor,
        groups is a dict with the name as key and a Group object as value'''
        self.groups = {}
        self.reverseGroups = {}
        self.noGroup = Group('No group', 'nogroup')
        self.contacts = {}
        if groups:
            self.setGroups(groups)
        self.lists = {}
        self.lists['Allow'] = []
        self.lists['Block'] = []
        self.lists['Reverse'] = []
        self.lists['Pending'] = []
        self.pendingNicks = {}
    def getGroupNames(self):
        '''Returns a list with the groups' names in the contact list'''
        return self.reverseGroups.keys()
    def setGroups(self, groupDict):
        '''set the dict'''
        self.groups = groupDict.copy()
        self.reverseGroups = {}
        for i in self.groups.keys():
            self.reverseGroups[self.groups[i].name] = self.groups[i]
    def addGroup(self, name, gid):
        group = Group(name, gid)
        self.setGroup(gid, group)
    def getGroup(self, id):
        if self.groups.has_key(id):
            return self.groups[id]
        else:
            common.debug('group not found, returning dummy group')
            return Group(id, 'dummy' + id)
    def setGroup(self, id, groupObject):
        self.groups[id] = groupObject
        self.reverseGroups[groupObject.name] = self.groups[id]
        for contact in groupObject.users.copy():
            self.addUserToGroup(contact, id)
    def removeGroup(self, group):
        if self.groups.has_key(group):
            for contact in self.groups[group].users.copy():
                self.removeUserFromGroup(contact, group)
            name = self.groups[group].name
            del self.groups[group]
            del self.reverseGroups[name]
    def renameGroup(self, id, newName):
        if self.groups.has_key(id):
            del self.reverseGroups[self.groups[id].name]
            self.groups[id].name = newName
            self.reverseGroups[newName] = self.groups[id]
    def getGroupId(self, name):
        if self.reverseGroups.has_key(name):
            return self.reverseGroups[name].id
        elif name == 'No group':
            return 'nogroup'
        else:
            return None
    def getGroupName(self, id):
        if id in self.groups:
            return self.groups[id].name
        elif id == 'nogroup':
            return 'No group'
        else:
            return 'dummy' + str(id)
    def addContact(self, contact):
        self.contacts[contact.email] = contact
        for id in contact.groups:
            if self.groups.has_key(id):
                self.groups[id].setUser(contact.email, contact)
        if contact.groups == []:
            self.noGroup.setUser(contact.email, contact)
    def addNewContact(self, email, groups=None):
        email = email.lower()
        if email in self.lists['Block']:
            contact = Contact(email, blocked=True)
        else:
            contact = Contact(email, allow=True)
        self.addContact(contact)
    def setContactIdXml(self, email, xml):
        '''Sets a contact's id from the add user soap response xml'''
        email = str(email).lower()
        if email in self.contacts:
            guid = xml.split('<guid>')[1].split('</guid>')[0]
            self.contacts[email].id = guid
        else:
            common.debug('Contact %s not in list' % email)
    def getContact(self, email):
        email = email.lower()
        if self.contacts.has_key(email):
            return self.contacts[email]
        else:
            common.debug('user %s not found, returning dummy user' % (email,))
            return self.getDummyContact(email)
    def getDummyContact(self, email):
        '''build a dummy contact with some data to be allowed to show data about
        contacts that we dont have i.e when someone add in a group chat someone that we dont have.'''
        email = email.lower()
        return Contact(email, '', email, '', '', 'NLN', False, False, False,
            True, True, False, dummy=True, clientid=0)
    def contact_exists(self, email): # breaking name conventions wdeaah
        return self.contacts.has_key(email.lower())
    def getContactStatus(self, email):
        email = email.lower()
        if self.contacts.has_key(email):
            return self.contacts[email].status
        else:
            return 'FLN'
    def setContactStatus(self, email, status):
        email = email.lower()
        if self.contacts.has_key(email):
            self.contacts[email].status = status
    def getContactHasSpace(self, email):
        email = email.lower()
        if self.contacts.has_key(email):
            return self.contacts[email].space
        else:
            return False
    def getContactHasMobile(self, email):
        email = email.lower()
        if self.contacts.has_key(email):
            return self.contacts[email].mobile
        else:
            return False
    def getContactIsBlocked(self, email):
        email = email.lower()
        if self.contacts.has_key(email):
            return self.contacts[email].blocked
        else:
            return False
    def setContactIsBlocked(self, email, value):
        email = email.lower()
        if self.contacts.has_key(email):
            self.contacts[email].blocked = value
        else:
            pass
    def getContactIsAllowed(self, email):
        email = email.lower()
        if self.contacts.has_key(email):
            return self.contacts[email].allow
        else:
            return True
    def setContactIsAllowed(self, email, value):
        email = email.lower()
        if self.contacts.has_key(email):
            self.contacts[email].allow = value
    def getContactNick(self, email, escaped=False):
        email = email.lower()
        if self.contacts.has_key(email):
            nick = self.contacts[email].nick
        elif self.pendingNicks.has_key(email):
            nick = self.pendingNicks[email]
        else:
            nick = email
        if escaped:
            return common.escape(nick)
        else:
            return nick
    def setContactNick(self, email, value):
        email = email.lower()
        if self.contacts.has_key(email):
            self.contacts[email].nick = value
    def getContactPersonalMessage(self, email, escaped=False):
        email = email.lower()
        if self.contacts.has_key(email):
            if escaped:
                return common.escape(self.contacts[email].personalMessage)
            else:
                return self.contacts[email].personalMessage
    def setContactPersonalMessage(self, email, value):
        email = email.lower()
        if self.contacts.has_key(email):
            self.contacts[email].personalMessage = value
    def getContactAlias(self, email, escaped=False):
        email = email.lower()
        if self.contacts.has_key(email):
            if escaped:
                return common.escape(self.contacts[email].alias)
            else:
                return self.contacts[email].alias
        else:
            return ''
    def setContactAlias(self, email, value):
        email = email.lower()
        if self.contacts.has_key(email):
            self.contacts[email].alias = value
    def getContactNameToDisplay(self, email):
        email = email.lower()
        displayName = self.getContactAlias(email, True)
        if displayName == '':
            displayName = self.getContactNick(email, True)
        return displayName
    def getContactId(self, email):
        email = email.lower()
        if self.contacts.has_key(email):
            return self.contacts[email].id
        else:
            return ''
    def getContactGroupIds(self, email):
        '''return a list with the group ids or an empty list'''
        email = email.lower()
        if self.contacts.has_key(email) and len(self.contacts[email].groups) > 0:
            return self.contacts[email].groups
        return []
    def unblockContact(self, email):
        email = email.lower()
        if self.contacts.has_key(email):
            self.contacts[email].blocked = False
            self.contacts[email].allow = True
    def blockContact(self, email):
        email = email.lower()
        if self.contacts.has_key(email):
            self.contacts[email].blocked = True
            self.contacts[email].allow = False
    def removeUserFromGroup(self, user, group):
        user = str(user).lower()
        group = str(group)
        if self.groups.has_key(group):
            contact = self.groups[group].getUser(user)
            if contact != None:
                contact.removeGroup(group)
                if contact.groups == []:
                    self.noGroup.setUser(user, contact)
                self.groups[group].removeUser(user)
            else:
                common.debug('Contact %s not in group %s' % (user, group))
        else:
            common.debug('Group %s not found' % group)
    def removeContact(self, contactMail):
        contactMail = str(contactMail).lower()
        if self.contacts.has_key(contactMail):
            contact = self.contacts[contactMail]
            for group in contact.groups:
                self.groups[group].removeUser(contactMail)
            if len(contact.groups) == 0:
                self.noGroup.removeUser(contactMail)
            del self.contacts[contactMail]
        else:
            common.debug('Contact %s not in list' % contactMail)
    def addUserToGroup(self, user, group):
        user = str(user).lower()
        group = str(group)
        if self.groups.has_key(group) and self.contacts.has_key(user):
            contact = self.contacts[user]
            if len(contact.groups) == 0:
                self.noGroup.removeUser(user)
            groupObj = self.groups[group]
            contact.addGroup(group)
            groupObj.setUser(user, contact)
        elif not self.contacts.has_key(user):
            common.debug('Contact %s not in list' % user)
        elif group not in self.groups:
            common.debug('Group %s not found' % group)
    def updateMemberships(self):
        '''Updates contact membership info according to self.lists'''
        for email in self.contacts:
            self.contacts[email.lower()].reverse = (email in self.lists['Reverse'])
            self.contacts[email.lower()].allow = (email in self.lists['Allow'])
            self.contacts[email.lower()].blocked = (email in self.lists['Block'])
    def getADL(self):
        '''Create a XML String with all the contacts we have for
        the initial ADL Command'''
        contacts = {}
        for user in self.contacts.keys():
            l = 0
            if self.getContact(user).allow:
                l = 3
            if self.getContact(user).blocked:
                l = 5
            contacts[user] = l
        return self.buildDL(contacts, initial=True)
    def buildDL(self, contacts, initial=False):
        '''return a list of XML for the DL command, is a list because each DL
        should be less than 7500 bytes
        contacts is a dict {user: type}'''
        domains = {}
        for i in contacts.keys():
            (user, domain) = i.split('@')
            if domains.has_key(domain):
                domains[domain].append(user)
            else:
                domains[domain] = [user]
        xmlDomains = []
        for i in domains.keys():
            users = ''
            for j in domains[i]:
                l = contacts[j + '@' + i]
                if l > 0:
                    users += '<c n="' + j + '" l="' + str(l) + '" t="1" />'
                if len(users) + len('<d n="' + i + '"></d>') > 7200:
                    xmlDomains.append('<d n="' + i + '">' + users + '</d>')
                    users = ''
            if len(users) > 0:
                xmlDomains.append('<d n="' + i + '">' + users + '</d>')
        adls = []
        full = False
        while xmlDomains:
            if initial:
                xml = '<ml l="1">'
            else:
                xml = '<ml>'
            for i in range(len(xmlDomains)):
                domain = xmlDomains.pop()
                if len(xml) + len(domain) < 7400:
                    xml += domain
                else:
                    xml += '</ml>'
                    adls.append(xml)
                    xmlDomains.append(domain)
                    full = True
                    break
            if not full:
                xml += '</ml>'
                adls.append(xml)
            else:
                full = False
        return adls
    def getOnlineUsers(self):
        '''return a list of online users'''
        ret = []
        for i in self.contacts.keys():
            if self.getContactStatus(i) != 'FLN':
                ret.append([i, self.getContactStatus(i)])
        return ret
    def getOnlineUsersDict(self):
        dictionary = {}
        for i in self.contacts.keys():
            if self.getContactStatus(i) != 'FLN':
                dictionary[i] = self.getContact(i)
        return dictionary
    def getOnOffUsersRelationByGroup(self, groupName):
        groupSizeStr = ''
        groupObject = None
        if groupName == 'No group':
            groupObject = self.noGroup
        else:
            try:
                groupObject = self.reverseGroups[groupName]
            except KeyError:
                common.debug('Group %s not found' % groupName)
        if groupObject != None:
            groupSize = groupObject.getSize()
            usersOnline = groupObject.getOnlineUsersNumber()
        return usersOnline, groupSize
    def getContactList(self, showOffline = True, showEmptyGroups = False, \
            orderByStatus = False):
        '''return a dictionarie with the contact list sorted acording the parameters'''
        cl = {}
        if orderByStatus:
            cl['offline'] = {}
            cl['online'] = {}
            for email in self.contacts:
                status = self.getContactStatus(email)
                if status == 'FLN' and not showOffline:
                    continue
                elif status == 'FLN':
                    cl['offline'][email] = self.contacts[email]
                else:
                    cl['online'][email] = self.contacts[email]
        else:
            for i in self.reverseGroups:
                cl[i] = {}
            cl['No group'] = {}
            for email in self.contacts:
                contactGroups = []
                for id in self.getContactGroupIds(email):
                    contactGroups += [self.groups[id].name]
                if len(contactGroups) == 0: # email doesn't belong to any group
                    contactGroups = ['No group']
                for group in contactGroups:
                    if showOffline:
                            cl[group][email] = self.contacts[email]
                    elif self.getContactStatus(email) != 'FLN':
                            cl[group][email] = self.contacts[email]
        if not showEmptyGroups:
            for i in cl.keys():
                if len(cl[i]) == 0:
                    del cl[i]
        return cl
class ContactNotInListError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return 'Contact ' + repr(self.value) + ' is not in the list'
class ContactNotInGroupError(Exception):
    def __init__(self, value, group):
        self.value = value
        self.group = group
    def __str__(self):
        return 'Contact ' + repr(self.value) + ' is not in this group: ' \
                        + str(self.group)
class GroupNotFoundError(Exception):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        return 'Group ' + repr(self.value) + ' does not exist'
