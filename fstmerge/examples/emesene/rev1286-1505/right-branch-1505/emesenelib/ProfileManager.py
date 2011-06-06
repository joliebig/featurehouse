import gobject
import urllib
import SoapManager
import ContactData
import XmlTemplates
import XmlParser
import common
class ProfileManager(gobject.GObject):
    '''this class has all the methods to
    modify the contacts, groups, nick and stuff
    through SOAP, its made to make Msnp
    more modular, also this class can be
    changed for other implementation later'''
    __gsignals__ = {
        'user-attr-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT,)),
        'group-attr-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT,gobject.TYPE_PYOBJECT)),
        'contact-added' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT,gobject.TYPE_PYOBJECT,
            gobject.TYPE_PYOBJECT,gobject.TYPE_PYOBJECT,
            gobject.TYPE_PYOBJECT,gobject.TYPE_PYOBJECT,
            gobject.TYPE_PYOBJECT,)),
        'contact-removed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT,)),
        'contact-attr-changed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT,gobject.TYPE_PYOBJECT, 
                gobject.TYPE_PYOBJECT,)),
        'group-added' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT,gobject.TYPE_PYOBJECT)),
        'group-removed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT,)),
        'group-renamed' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT,gobject.TYPE_PYOBJECT)),
        'contact-added-to-group' : (gobject.SIGNAL_RUN_LAST, gobject.TYPE_NONE,
            (gobject.TYPE_PYOBJECT,gobject.TYPE_PYOBJECT)),
        'contact-removed-from-group' : (gobject.SIGNAL_RUN_LAST, 
            gobject.TYPE_NONE, (gobject.TYPE_PYOBJECT,gobject.TYPE_PYOBJECT)),
    }
    def __init__(self):
        gobject.GObject.__init__(self)
    def getMembershipListSync(self):
        '''request the membership list dont return until we get it'''
        common.debug('request Membership list')
        request = SoapManager.SoapRequest(\
            'http://www.msn.com/webservices/AddressBook/FindMembership', \
            'contacts.msn.com', 443, '/abservice/SharingService.asmx', \
            XmlTemplates.membershipList)
        SoapManager.put(request)
        response = SoapManager.get()
        return self.onGetMembershipList(response)
    def getMembershipListAsync(self):
        '''request the membership list return and put a callback'''
        common.debug('request Membership list')
        request = SoapManager.SoapRequest(\
            'http://www.msn.com/webservices/AddressBook/FindMembership', \
            'contacts.msn.com', 443, '/abservice/SharingService.asmx', \
            XmlTemplates.membershipList, self.onGetMembershipList)
        SoapManager.put(request)
    def onGetMembershipList(self, response):
        '''method called when we receive the membership list'''
        common.debug(str(response.status))
        if response.status[0] == 200:
            self.setMembershipListXml(response.body )
            self.newCacheFile(self.user + '_ml.xml', response.body)
            return True
        else:
            return False
    def getAddressBookSync(self):
        '''request the address book return and put a callback'''
        common.debug('request Address book')
        request = SoapManager.SoapRequest(\
            'http://www.msn.com/webservices/AddressBook/ABFindAll', \
            'contacts.msn.com', 443, \
            '/abservice/abservice.asmx', XmlTemplates.addressBook)
        SoapManager.put(request)
        response = SoapManager.get()
        common.debug(str(response.status))
        if response.status[0] == 200:
            self.newCacheFile(self.user + '_ab.xml', response.body)
    def getAddressBookAsync(self):
        '''request the address book dont return until we get it'''
        common.debug('request Address book')
        request = SoapManager.SoapRequest(
            'http://www.msn.com/webservices/AddressBook/ABFindAll', \
            'contacts.msn.com', 443, '/abservice/abservice.asmx', \
            XmlTemplates.addressBook, self.onGetAddressBook)
        SoapManager.put(request)
    def onGetAddressBook(self, response):
        common.debug(str(response.status))
        if response.status[0] == 200:
            self.newCacheFile(self.user + '_ab.xml', response.body)
    def getDynamicItemsSync(self):
        '''request dynamic items (spaces and all that) dont return
        until we get that'''
        common.debug('request Dynamic Items')
        request = SoapManager.SoapRequest(\
            'http://www.msn.com/webservices/AddressBook/ABFindAll', \
            'contacts.msn.com', 443, '/abservice/abservice.asmx', \
            XmlTemplates.dynamicItems)
        SoapManager.put(request)
        response = SoapManager.get()
        return self.onGetDynamicItems(response)
    def getDynamicItemsAsync(self):
        '''request dynamic items (spaces and all that) return
        and put a callback'''
        common.debug('request Dynamic Items')
        request = SoapManager.SoapRequest(
            'http://www.msn.com/webservices/AddressBook/ABFindAll', \
            'contacts.msn.com', 443, '/abservice/abservice.asmx', \
            XmlTemplates.dynamicItems, self.onGetDynamicItems)
        SoapManager.put(request)
    def onGetDynamicItems(self, response):
        common.debug(str(response.status))
        if response.status[0] == 200:
            self.newCacheFile(self.user + '_di.xml', response.body)
            self.setDynamicItemsXml(response.body)
            self.getNickFromDynamicItems(response.body)
            self.changeNick(self.nick)
            self.emit('user-list-change')
            return True
        else:
            return False
    def getStorageAuthCache(self, email):
        common.debug('request storage auth cache ' + email)
        request = SoapManager.SoapRequest(\
            'http://www.msn.com/webservices/storage/w10/GetItemVersion', \
            'storage.msn.com', 80, '/storageservice/schematizedstore.asmx', \
            XmlTemplates.schematizedStore%(email), self.onGetStorageAuthCache)
        SoapManager.put(request)
    def onGetStorageAuthCache(self, response):
        common.debug(str(response.status) + "\n" + response.body)
    def getUserProfile(self, email, storageAuthCache = None):
        if storageAuthCache == None:
            self.getStorageAuthCache(email)
        else:
            common.debug('request user profile ' + email)
            cid = str(self.contactManager.getContact(email).cid)
            request = SoapManager.SoapRequest(
                'http://www.msn.com/webservices/spaces/v1/GetXmlFeed', \
                'cid-580B3F2402D4CDA4.cc.services.spaces.live.com', 80, \
                '/contactcard/contactcardservice.asmx', \
                XmlTemplates.space % (self.t, "", cid, storageAuthCache, "en-US"), \
                self.onGetUserProfile)
            SoapManager.put(request)
    def onGetUserProfile(self, response):
        common.debug(str(response.status) + "\n" + response.body)
    def getNickFromDynamicItems(self, dynamicItems):
        try:
            temp = dynamicItems.split('<contactType>Me</contactType>')\
                    [1].split('</displayName>')[0]
            nick = common.unescape(temp.split('<displayName>')[1])
        except:
            nick = self.user
        if nick != self.nick:
            oldNick = self.nick
            self.nick = nick
            self.emit('self-nick-changed', oldNick, self.nick)
    def addUser(self, email, group):
        '''add an user to the friend list'''
        if self.contactManager.contact_exists(email):
            return
        self.sendDL('ADL', email, '1')
        self.sendDL('ADL', email, '2')
        self.contactManager.addNewContact(email)
        request = SoapManager.SoapRequest(
            'http://www.msn.com/webservices/AddressBook/ABContactAdd', \
            'omega.contacts.msn.com', 443, '/abservice/abservice.asmx', \
            XmlTemplates.contactAdd % (email,), \
            self.onUserAdded, (email, group))
        SoapManager.put(request)
    def onUserAdded(self, response, email, group=''):
        '''this method is called when the addUser soapRequest get a response'''
        common.debug('add user: ' + email + ' ' + str(response.status))
        if response.status[0] == 200:
            self.contactManager.setContactIdXml(email, response.body)
            guid = response.body.split('<guid>')[1].split('</guid>')[0]
            self.emit('contact-added', email, guid, None, None, 'FLN', None,
                False)
            if group == '':
                self.emit('user-list-change')
            else:
                self.addUserToGroup(email, group)
        else:
            self.contactManager.removeContact(email)
            self.emit('user-list-change')
            self.emit('error', 'user-add-error', 
                _('User could not be added: %s') % \
                common.parseSoapFault(response))
    def removeUserFromGroup(self, user, group):
        '''remove user from a group'''
        contactID = self.contactManager.getContactId(user)
        sourceGid = self.contactManager.getGroupId(group)
        if sourceGid == '' or sourceGid == 'nogroup':
            return
        self.contactManager.removeUserFromGroup(user, sourceGid)
        self.emit('user-list-change')
        request = SoapManager.SoapRequest(
            'http://www.msn.com/webservices/AddressBook/ABGroupContactDelete', \
            'omega.contacts.msn.com', 443, '/abservice/abservice.asmx', \
            XmlTemplates.deleteUserFromGroup % (contactID,sourceGid), \
            self.onUserRemovedFromGroup, (sourceGid, group, user))
        SoapManager.put(request)
    def onUserRemovedFromGroup(self, response, groupId, group, user):
        common.debug('remove user from group: ' + str(response.status))
        if response.status[0] == 200:
            self.emit('contact-removed-from-group', user, group)
        else:
            self.contactManager.addUserToGroup(user, groupId)
            self.emit('user-list-change')
            self.emit('error', 'user-remove-error', _('User could not be remove: %s') % common.parseSoapFault(response))
    def addUserToGroup(self, user, group):
        '''add a user to a group'''
        gid = self.contactManager.getGroupId(group)
        contactID = self.contactManager.getContactId(user)
        if gid == None:
            common.debug('Group not found')
            return
        if gid in self.contactManager.getContact(user).groups:
            common.debug('User already in group')
            return
        if gid == 'nogroup':
            common.debug('Cannot move to no group')
            return            
        self.contactManager.addUserToGroup(user, gid)
        self.emit('user-list-change')
        request = SoapManager.SoapRequest(
            'http://www.msn.com/webservices/AddressBook/ABGroupContactAdd', \
            'omega.contacts.msn.com', 443, '/abservice/abservice.asmx', \
            XmlTemplates.moveUserToGroup % (gid, contactID), \
            self.onUserAddedToGroup, (user, group, gid))
        SoapManager.put(request)
    def onUserAddedToGroup(self, response, user, group, groupId):
        common.debug('add user to group: ' + str(response.status))
        if response.status[0] == 200:
            self.emit('contact-added-to-group', user, group)
        else:
            self.contactManager.removeUserFromGroup(user, groupId)
            self.emit('user-list-change')
            self.emit('error', 'user-add-to-group-error', \
                _('User could not be added to group: %s') % \
                common.parseSoapFault(response))
    def moveUserToGroup(self, user, srcGroup, destGroup, stage=0):
        '''move a user from a group erasing it from de source group'''
        if stage == 0:
            contactID = self.contactManager.getContactId(user)
            sourceGid = self.contactManager.getGroupId(srcGroup)
            destGid = self.contactManager.getGroupId(destGroup)
            if sourceGid == 'nogroup':
                self.addUserToGroup(user, destGroup)
                return
            if destGid == 'nogroup':
                self.removeUserFromGroup(user, srcGroup)
                return
            if srcGroup == destGroup:
                common.debug('src and dest groups are the same')
                return
            elif self.contactManager.getGroup(destGid).getUser(user) != None:
                common.debug('dest group already contain the user')
                return
            self.contactManager.removeUserFromGroup(user, sourceGid)
            self.contactManager.addUserToGroup(user, destGid)
            self.emit('user-list-change')
            request = SoapManager.SoapRequest(
                'http://www.msn.com/webservices/AddressBook/ABGroupContactDelete', \
                'omega.contacts.msn.com', 443, '/abservice/abservice.asmx', \
                XmlTemplates.deleteUserFromGroup%(contactID,sourceGid), \
                self.onMoveUserToGroup, (user, srcGroup, destGroup, 0))
            SoapManager.put(request)
        elif stage == 1:
            gid = self.contactManager.getGroupId(destGroup)
            contactID = self.contactManager.getContactId(user)
            request = SoapManager.SoapRequest(\
                'http://www.msn.com/webservices/AddressBook/ABGroupContactAdd', \
                'omega.contacts.msn.com', 443, '/abservice/abservice.asmx', \
                XmlTemplates.moveUserToGroup%(gid, contactID), \
                self.onMoveUserToGroup, (user, srcGroup, destGroup, 1))
            SoapManager.put(request)
    def onMoveUserToGroup(self, response, user, srcGroup, destGroup, stage):
        common.debug('move user (stage ' + str(stage) + '): ' \
                           + str(response.status))
        status = response.status
        if response.status[0] == 200:
            if stage == 0: # continue the moving procedure
                self.moveUserToGroup(user, srcGroup, destGroup, 1)
        else:
            sourceGid = self.contactManager.getGroupId(srcGroup)
            destGid = self.contactManager.getGroupId(destGroup)
            self.contactManager.removeUserFromGroup(user, destGid)
            self.contactManager.addUserToGroup(user, sourceGid)
            self.emit('user-list-change')
            self.emit('error', 'user-move-to-group-error', _('User could not be moved to group: %s') % common.parseSoapFault(response))
    def removeUser(self, email):
        '''remove an user from the friendr list'''
        self.sendDL('RML', email, '1')
        contact = self.contactManager.getContact(email)
        contactID = self.contactManager.getContactId(email)
        self.contactManager.removeContact(email)
        self.emit('user-list-change')
        request = SoapManager.SoapRequest(\
            'http://www.msn.com/webservices/AddressBook/ABContactDelete', \
            'omega.contacts.msn.com', 443, '/abservice/abservice.asmx', \
            XmlTemplates.contactRemove % (contactID,), \
            self.onUserRemoved, (email, contact))
        SoapManager.put(request)
    def onUserRemoved(self, response, email, contact):
        '''this method is called when the removeUser soapRequest get a response
        renes means if we have to renew the di,ml,ab data'''
        common.debug('remove user: ' + email + ' ' + str(response.status))
        if response.status[0] == 200:
            self.emit('contact-removed', email)
        else:
            self.contactManager.addContact(contact)
            self.emit('user-list-change')
            self.emit('error', 'user-remove-error', _('User could not be removed: %s') % common.parseSoapFault(response))
    def blockUser(self, email, stage=0):
        '''block an user'''
        if stage == 0:
            self.sendDL('RML', email, '2')
            self.sendDL('ADL', email, '4')
            self.contactManager.blockContact(email)
            contact = self.contactManager.getContact(email)
            self.emit('user-attr-changed', contact)
            request = SoapManager.SoapRequest(
                'http://www.msn.com/webservices/AddressBook/DeleteMember', \
                'omega.contacts.msn.com', 443, '/abservice/SharingService.asmx', \
                XmlTemplates.deleteMember%('BlockUnblock', 'Allow', email), \
                self.onUserBlocked, (email, 0))
            SoapManager.put(request)
        elif stage == 1:
            request = SoapManager.SoapRequest(
                'http://www.msn.com/webservices/AddressBook/AddMember', \
                'omega.contacts.msn.com', 443, '/abservice/SharingService.asmx', \
                XmlTemplates.addMember%('BlockUnblock', 'Block', email), \
                self.onUserBlocked, (email, 1))
            SoapManager.put(request)
    def onUserBlocked(self, response, email, stage):
        '''this method is called when the blockUser soapRequest get a response'''
        common.debug('block user (stage ' + str(stage) + '): ' \
                           + str(response.status))
        if response.status[0] == 200:
            if stage == 0: # continue the blocking process
                self.blockUser(email, 1)
            self.emit('contact-attr-changed', email, 'block', True)
        else:
            self.unblockUser(email, 0)
            self.contactManager.unblockContact(email)
            contact = self.contactManager.getContact(email)
            self.emit('user-attr-changed', contact)
    def unblockUser(self, email, stage=0):
        '''unblock an user'''
        if stage == 0:
            self.sendDL('RML', email, '4')
            self.sendDL('ADL', email, '2')
            self.contactManager.unblockContact(email)
            contact = self.contactManager.getContact(email)
            self.emit('user-attr-changed', contact)
            request = SoapManager.SoapRequest(\
                'http://www.msn.com/webservices/AddressBook/DeleteMember', \
                'omega.contacts.msn.com', 443, '/abservice/SharingService.asmx', \
                XmlTemplates.deleteMember%('BlockUnblock', 'Block', email), \
                self.onUserUnblocked, (email, 0))
            SoapManager.put(request)
        elif stage == 1:
            request = SoapManager.SoapRequest(
                'http://www.msn.com/webservices/AddressBook/AddMember', \
                'omega.contacts.msn.com', 443, '/abservice/SharingService.asmx', \
                XmlTemplates.addMember%('BlockUnblock', 'Allow', email), \
                self.onUserUnblocked, (email, 1))
            SoapManager.put(request)
    def onUserUnblocked(self, response, email, stage):
        '''this method is called when the unblockUser soapRequest get a response'''
        common.debug('unblock user (stage ' + str(stage) + '): ' \
                           + str(response.status))
        if response.status[0] == 200:
            if stage == 0:
                self.unblockUser(email, 1)
            self.emit('contact-attr-changed', email, 'block', False)
        else:
            self.contactManager.blockContact(email)
            contact = self.contactManager.getContact(email)
            self.emit('user-attr-changed', contact)
    def addGroup(self, group):
        '''add a group to the group list'''
        if self.contactManager.getGroupId(group) != None:
            common.debug('Unable to add: Group "' + group \
                               + '" already exists')
            return
        name = group.replace(' ', '%20')
        request = SoapManager.SoapRequest(
                    'http://www.msn.com/webservices/AddressBook/ABGroupAdd', \
                    'omega.contacts.msn.com', 443, '/abservice/abservice.asmx', \
                    XmlTemplates.addGroup % (group,), \
                    self.onGroupAdded, (group,))
        SoapManager.put(request)
    def onGroupAdded(self, response, group):
        '''this method is called when the addGroup soap request get a response'''
        common.debug('add group ' + str(response.status))
        if response.status[0] == 200:
            try:
                gid = response.body.split('<guid>')[1].split('</guid>')[0]
                self.contactManager.addGroup(group, gid)
                self.emit('group-added', group, gid)
                self.emit('user-list-change')
            except IndexError, e:
                common.debug('cannot add group to userlist')
                common.debug(str(e))
        else:
            self.emit('error', 'group-add-error', _('Group could not be added: %s') % common.parseSoapFault(response))
    def removeGroup(self, group):
        '''remove a group from the group list'''
        gid = self.contactManager.getGroupId(group)
        if gid:
            groupObj = self.contactManager.getGroup(gid)
            self.contactManager.removeGroup(gid)
            self.emit('user-list-change')
            request = SoapManager.SoapRequest(
                'http://www.msn.com/webservices/AddressBook/ABGroupDelete', \
                'omega.contacts.msn.com', 443, '/abservice/abservice.asmx', \
                XmlTemplates.deleteGroup % (gid,), \
                self.onGroupRemoved, (gid, groupObj))
            SoapManager.put(request)
        else:
            common.debug('Unable to remove: Group "' + group \
                               + '" does not exist')
    def onGroupRemoved(self, response, gid, group):
        '''this method is called when the removeGroup soap request get a response'''
        common.debug('remove group ' + str(response.status))
        if response.status[0] == 200:
            self.emit('group-removed', group)
        else:
            self.contactManager.setGroup(gid, group)
            self.emit('user-list-change')
            self.emit('error', 'group-remove-error', _('Group could not be removed: %s') % common.parseSoapFault(response))
    def renameGroup(self, oldGroup, newGroup):
        '''rename a group from the group list'''
        if oldGroup == newGroup:
            common.debug('oldgroup and new group are the same')
            return
        if self.contactManager.getGroupId(newGroup) != None:
            common.debug('That group name is already in use')
            return
        gid = self.contactManager.getGroupId(oldGroup)
        if gid == None:
            common.debug('The specified group does not exist')
            return
        else:
            self.contactManager.renameGroup(gid, newGroup)
            group = self.contactManager.getGroup(gid)
            objOldGroup = ContactData.Group(oldGroup)
            self.emit('group-attr-changed', objOldGroup, group)
            request = SoapManager.SoapRequest(
                'http://www.msn.com/webservices/AddressBook/ABGroupUpdate', \
                'omega.contacts.msn.com', 443, '/abservice/abservice.asmx', \
                XmlTemplates.renameGroup % (gid, common.escape(newGroup)), \
                self.onGroupRenamed, (oldGroup, newGroup))
            SoapManager.put(request)
    def onGroupRenamed(self, response, oldGroup, newGroup):
        '''this method is called when the renameGroup soap request get a response'''
        common.debug('rename group ' + str(response.status))
        if response.status[0] == 200:
            self.emit('group-renamed', oldGroup, newGroup)
        else:
            gid = self.contactManager.getGroupId(newGroup)
            self.contactManager.renameGroup(gid, oldGroup)
            group = self.contactManager.getGroup(gid)
            objOldGroup = ContactData.Group(newGroup)
            self.emit('group-attr-changed', objOldGroup, group)
            self.emit('error', 'group-rename-error', _('Group could not be renamed: %s') % common.parseSoapFault(response))
    def changeNick(self, nick, user='Me', initial=False):
        '''change the nick, type can be Me or Normal'''
        nick = nick.decode('utf-8', 'replace').encode('utf-8')
        if user == 'Me':
            if not initial and self.nick == nick:
                common.debug('trying to set the same nick')
                return
            if len(nick) > 129:
                return
            oldNick = self.nick
            self.nick = nick
            self.emit('self-nick-changed', oldNick, self.nick)
            self.socket.sendCommand("PRP", "MFN " + urllib.quote(nick))
            nick = common.escape(nick)
            if not initial:
                request = SoapManager.SoapRequest(
                    'http://www.msn.com/webservices/AddressBook/ABContactUpdate', \
                    'omega.contacts.msn.com', 443, '/abservice/abservice.asmx', \
                    XmlTemplates.changeNick % (user, nick), \
                    self.onNickChanged, (self.user, oldNick))
        else:
            oldAlias = self.contactManager.getContactAlias(user)
            self.contactManager.setContactAlias(user, nick)
            contact = self.contactManager.getContact(user)
            self.emit('user-attr-changed', contact)
            contactID = str(self.contactManager.getContactId(user))
            nick = str(common.escape(nick))
            if not initial:
                request = SoapManager.SoapRequest(
                    'http://www.msn.com/webservices/AddressBook/ABContactUpdate', \
                    'omega.contacts.msn.com', 443, '/abservice/abservice.asmx', \
                    str(XmlTemplates.renameContact % (contactID, nick)), \
                    self.onNickChanged, (user, oldAlias))
        if not initial:
            SoapManager.put(request)
    def onNickChanged(self, response, user, oldNick):
        '''this method is called when the changeNick soap request get a response'''
        common.debug('change nick ' + str(response.status))
        if response.status[0] != 200:
            if user == 'Me':
                self.emit('self-nick-changed', self.nick, oldNick)
            else:
                self.emit('contact-attr-changed', user, 'alias', oldNick)
                self.contactManager.setContactAlias(user, oldNick)
                contact = self.contactManager.getContact(user)
                self.emit('user-attr-changed', contact)
            self.emit('error', 'nick-change-error', _('Nick could not be changed: %s') % common.parseSoapFault(response))
    def updateUUX(self):
        '''update personal message and current media'''
        pm = self.personalMessage
        cm = self.currentMedia
        pm = pm.decode('utf-8', 'replace').encode('utf-8')
        cm = cm.decode('utf-8', 'replace').encode('utf-8')
        self.socket.sendPayloadCommand('UUX', '', \
            '<Data><PSM>' + common.escape(pm) + '</PSM>' + \
            '<CurrentMedia>' + common.escape(cm) + '</CurrentMedia>' + \
            '<MachineGuid></MachineGuid></Data>')
    def changePersonalMessage(self, pm):
        '''change the personal message'''
        if self.personalMessage != pm:
            self.personalMessage = pm
            self.updateUUX()
            self.emit('self-personal-message-changed', self.user, pm)
        else:
            common.debug("duplicate pm")
    def changeCurrentMedia(self, cm, dict=None):
        '''change the current media'''
        if self.currentMedia != cm:
            self.currentMedia = cm
            self.updateUUX()
            self.emit('self-current-media-changed', self.user, cm, dict)
        else:
            common.debug("duplicate cm")
    def getUserDisplayName(self, mail):
        '''return the user display name or just the mail if it cant be found'''
        mail = mail.lower()
        if mail == self.user:
            return self.nick
        else:
            alias = self.contactManager.getContactAlias(mail)
            if alias:
                return alias
            else:
                return self.contactManager.getContactNick(mail)
    def setDynamicItemsXml(self, xml):
        '''modify the structure with a new DynamicItems xml'''
        contacts = {}
        for (mail, contact) in self.contactManager.contacts.iteritems():
            contacts[mail] = contact
        self.contactManager.groups = {}
        self.contactManager.contacts = {}
        dinamicItems = XmlParser.DynamicParser(xml)
        for i in dinamicItems.groups:
            groupId = i['groupId']
            name = i['name']
            if groupId not in self.contactManager.groups:
                self.contactManager.setGroup(groupId , 
                    ContactData.Group(name, groupId))
                self.emit('group-added', name, groupId)
        for i in dinamicItems.contacts:
            if 'isMessengerUser' in i and 'passportName' in i and \
               i['isMessengerUser'] == 'true':
                email = i['passportName'].lower()
                contact = ContactData.Contact(email)
            else:
                continue
            try:
                contactId = i['contactId']
                cid = i['CID']
                contact.id = contactId
                contact.cid = cid
                groups = []
                for guid in i['groupIds']:
                    groups.append(guid)
                contact.groups = groups
                for j in i['Annotations']:
                    try:
                        if j['Name'] == 'AB.NickName':
                            alias = j['Value']
                            contact.alias = urllib.unquote(alias)
                    except:
                        pass
                displayName = i['displayName']
                contact.nick = urllib.unquote(displayName)
                isMobileIMEnabled = i['isMobileIMEnabled']
                contact.mobile = isMobileIMEnabled == 'true'
                hasSpace = i['hasSpace']
                contact.space = hasSpace == 'true'
            except KeyError:
                continue
            if email in contacts:
                contact.status = contacts[email].status
                contact.nick = contacts[email].nick
                contact.personalMessage = contacts[email].personalMessage
                contact.msnobj = contacts[email].msnobj
            self.contactManager.addContact(contact)
            self.emit('contact-added', contact.email, contact.id, contact.nick,
                contact.personalMessage, contact.status, contact.alias,
                contact.blocked)
            for group_id in contact.groups:
                self.emit('contact-added-to-group', contact.email, 
                    self.contactManager.getGroup(group_id).name)
        self.contactManager.updateMemberships()
    def setMembershipListXml(self, xml):
        '''modify the structure with a new MembershipList xml
        if it is the first xml that you send, send first the dynamic items'''
        self.contactManager.lists['Allow'] = []
        self.contactManager.lists['Block'] = []
        self.contactManager.lists['Reverse'] = []
        self.contactManager.lists['Pending'] = []
        ml = XmlParser.MembershipParser(xml)
        for i in ml.memberships:
            memberRole = i['MemberRole']
            for j in i['Members']:
                try:
                    email = j['PassportName'].lower()
                    if email not in self.contactManager.lists[memberRole]:
                        self.contactManager.lists[memberRole].append(email)
                    else:
                        pass
                    if memberRole == 'Pending' and 'DisplayName' in j: 
                        self.contactManager.pendingNicks[email] = \
                            j['DisplayName']
                except Exception, e:
                    pass
gobject.type_register(ProfileManager)
