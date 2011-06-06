import gtk
import time
import pango
import gobject
import os
import UserMenu
import GroupMenu
import TreeViewTooltips
from Parser import UserListDataType
import SmileyRenderer
import FancyAvatarRenderer
import emesenelib.common
from emesenelib.common import escape, debug
import traceback
class UserListModelTree(gtk.TreeStore):
    '''this class is the model for a tree like userlist'''
    def __init__(self):
        gtk.TreeStore.__init__(self, gtk.gdk.Pixbuf, 
            gobject.TYPE_PYOBJECT, str, str, gobject.TYPE_PYOBJECT, 
            bool, int, int, bool, str)
class UserListModelList(gtk.ListStore):
    '''this class is the model for a flat userlist'''
    def __init__(self):
        gtk.ListStore.__init__(self, gtk.gdk.Pixbuf, 
            gobject.TYPE_PYOBJECT, str, str, gobject.TYPE_PYOBJECT, 
            bool, int, int, bool, str)
class UserList(gtk.TreeView):
    '''the userlist widget'''
    PIX = 0
    LABEL = 1
    ID = 2
    TYPE = 3
    OBJ = 4
    SHOW = 5
    OFFLINE_COUNT = 6   # for group rows
    NOTOFFLINE_FLAG = 6    # for contact rows
    TOTAL_COUNT = 7
    BLOCKED = 8
    STATUS = 9
    __gsignals__ = { 'item-selected' : (gobject.SIGNAL_RUN_LAST, \
        gobject.TYPE_NONE, (gobject.TYPE_PYOBJECT,) * 3),
    }
    def __init__(self, controller, theme, config, isTreeStore = True):
        gtk.TreeView.__init__(self)
        self.theme = theme
        self.config = config
        self.controller = controller
        self.unifiedParser = controller.unifiedParser
        self.smileysCache = {}
        self.isTreeStore = isTreeStore
        if isTreeStore:
            self.model = UserListModelTree()
        else:
            self.model = UserListModelList()
        self.tooltips = TreeViewTooltips.TreeViewTooltips(self.theme, 
            self, UserList.OBJ, UserList.ID, UserList.TYPE)
        self.treeModelFilter = self.model.filter_new()
        self.treeModelFilter.set_visible_func(self.visible_func)
        self.set_model(self.treeModelFilter)
        self.filterText = ''
        self.groupState = {}
        self.restoreGroupState()
        statusColor = self.personalMessageColor
        self.userLabel = '%s' +\
        '\n<span size="small" foreground="' + statusColor + '">%s%s</span>'
        self.crt = SmileyRenderer.SmileyRenderer()
        self.crp = FancyAvatarRenderer.FancyAvatarRenderer(self.controller)
        column = gtk.TreeViewColumn()
        column.set_title('Icons & Text')
        column.set_expand(True)
        expColumn = gtk.TreeViewColumn()
        expColumn.set_max_width(16)       
        self.append_column(expColumn)
        self.append_column(column)
        self.set_expander_column(expColumn)
        column.pack_start(self.crp, False)
        column.pack_start(self.crt)
        column.add_attribute(self.crp, 'pixbuf', UserList.PIX)
        column.add_attribute(self.crp, 'blocked', UserList.BLOCKED)
        column.add_attribute(self.crp, 'status', UserList.STATUS)
        column.set_attributes(self.crt, markup=UserList.LABEL, 
            obj=UserList.OBJ)
        column.add_attribute(self.crp, 'visible', UserList.SHOW)
        self.set_search_column(UserList.ID)
        self.set_headers_visible(False)
        if self.isTreeStore:
            self.TARGETS = [('TREE_MODEL_ROW', gtk.TARGET_SAME_WIDGET, 0),
                            ('text/plain',0, 1),
                            ('TEXT', 0, 2),
                            ('STRING', 0, 3),
                            ('COMPOUND_TEXT', 0, 4),
                            ('UTF8_STRING', 0, 5)]
            self.enable_model_drag_source( gtk.gdk.BUTTON1_MASK|
                gtk.gdk.CONTROL_MASK, self.TARGETS, 
                gtk.gdk.ACTION_DEFAULT|gtk.gdk.ACTION_MOVE|
                gtk.gdk.ACTION_COPY)
            self.enable_model_drag_dest(self.TARGETS, 
                gtk.gdk.ACTION_DEFAULT )
            self.connect("drag_data_get", self.onDragDataGet)
            self.connect("drag_data_received", self.onDragDataReceived)
            self.connect("drag_drop", self.onDragDrop)
        self.connect("row_activated" , self.onRowActivated)
        self.connect("button-press-event" , self.onButtonPress)
        self.connect("row-expanded" , self.onRowExpanded)
        self.connect("row-collapsed" , self.onRowCollapsed)
        self.last_fill = 0
        if os.name != 'nt':  # gtk bug here?
            self.model.set_sort_func(UserList.OBJ, self.sortMethod)
            self.model.set_sort_column_id(UserList.OBJ, gtk.SORT_ASCENDING)
    def compareContact(self, x, y, status, type):
        def _getPriority(status, contact):
            status_priority = {
                'NLN':        0,
                'AWY':        2,
                'BSY':        6,
                'BRB':        3,
                'PHN':        4,
                'LUN':        5,
                'HDN':        7,
                'IDL':        1,
                'FLN':        8,
            }
            if not status:
                if contact.status == 'FLN':
                    return 8
                else:
                    return 0
            else:
                return status_priority[contact.status]
        if type:
            value = lambda x: x.email
        else:
            value = lambda x: x.nick
        if _getPriority(status, x) > _getPriority(status,y):
            return 1
        elif _getPriority(status, x) < _getPriority(status,y):
            return -1
        else:
            if value(x) == value(y):
                return 0
            elif value(x) > value(y):
                return 1
            else:
                return -1
    def sortMethod(self, treemodel, iter1, iter2, user_data=None):
        ''' method used for sorting the userlist'''
        status = self.sortNickGroupByStatusPriority
        type = self.sortNickGroupByContact
        obj1 = self.model[iter1][UserList.OBJ]
        obj2 = self.model[iter2][UserList.OBJ]
        if self.model[iter1][UserList.TYPE] == 'group' or \
            self.model[iter2][UserList.TYPE] == 'group':
            return -1
        try:
            return  self.compareContact(obj1, obj2, status, type)
        except:
            return -1
    def onDragDataGet(self, treeview, context, selection, target_id, etime):
        (rowType, contact) = self.getSelected()
        if rowType == 'user':
            if target_id == 0:
                selection.set('STRING', 8, contact.email + '|' + self.getGroupSelected().name)
            else:
                selection.set('STRING', 8, contact.nick + ' <' + contact.email + '>')
        else:
            if target_id == 0:
                selection.set('STRING', 8, '')
            else:
                selection.set('STRING', 8, self.getGroupSelected().name)
    def onDragDrop(self, widget, context, x, y, time):
        widget.drag_get_data( context, context.targets[0], time )
        return True
    def onDragDataReceived(self, treeview, context, x, y, selection, 
        info, etime):
        if self.orderByStatus or selection.data == '':
            context.finish( False, False, etime )
            return
        if info != 0:
            context.finish( False, False, etime )
            return
        model = treeview.get_model()
        mail, group = selection.data.split('|')
        drop_info = treeview.get_dest_row_at_pos(x, y)
        if not drop_info:
            context.finish( False, False, etime )
            return
        path, position = drop_info
        iter = model.get_iter(path)
        type, object = self.getTupleByIter(iter)
        if type == 'group':
            destGroup = object
        elif type == 'user':
            destGroup = self.getGroupSelected(iter)
        if context.action == gtk.gdk.ACTION_COPY:
            self.controller.contacts.add_to_group(mail, destGroup.name)
        else:
            self.controller.contacts.move_to_group(mail, group, 
                destGroup.name)
        context.finish( True, False, etime )
    def getSortNickGroupByStatusPriority(self):
        return self.config.user['sortNickGroupByStatusPriority']
    sortNickGroupByStatusPriority = property(fget=getSortNickGroupByStatusPriority)
    def getSortNickGroupByContact(self):
        return self.config.user['sortNickGroupByContact']
    sortNickGroupByContact = property(fget=getSortNickGroupByContact)
    def getShowByNick(self):
        return self.config.user['showByNick']
    showByNick = property(fget=getShowByNick)
    def getOrderByStatus(self):
        return self.config.user['orderByStatus']
    orderByStatus = property(fget=getOrderByStatus)
    def getShowOffline(self):
        return self.config.user['showOffline']
    showOffline = property(fget=getShowOffline)
    def getShowEmptyGroups(self):
        return self.config.user['showEmptyGroups']
    showEmptyGroups = property(fget=getShowEmptyGroups)
    def getShowCountContact(self):
        return self.config.user['showCountContact']
    showCountContact = property(fget=getShowCountContact)
    def getAvatarsInUserList(self):
        return self.config.user['avatarsInUserList']
    avatarsInUserList = property(fget=getAvatarsInUserList)
    def getPersonalMessageColor(self):
        return self.config.user['personalMessageColor']
    personalMessageColor = property(fget=getPersonalMessageColor)
    def getSmallIcons(self):
        return self.config.user['smallIcons']
    smallIcons = property(fget=getSmallIcons)
    def getParseSmilies(self):
        return self.config.user['parseSmilies']
    parseSmilies = property(fget=getParseSmilies)
    def getUserListAvatarSize(self):
        return self.config.user['userListAvatarSize']
    avatarSize = property(fget=getUserListAvatarSize)
    def visible_func(self, model, iter):
        obj = model[iter][UserList.OBJ]
        text = self.filterText.lower()
        if text == '' and obj and not self.showOffline and \
           model[iter][UserList.TYPE] == 'user' and obj.status == 'FLN':
            return False
        elif model[iter][UserList.TYPE] == 'group':
            totalCount = model[iter][UserList.TOTAL_COUNT]
            offlineCount = model[iter][UserList.OFFLINE_COUNT]
            if totalCount == offlineCount:
                if (self.showOffline and totalCount > 0) or \
                   self.showEmptyGroups or text != '':
                    return True
                else:
                    return False
            return True
        elif text == '':
            return True
        elif obj and \
             (obj.email.lower().find(text) != -1 or \
              obj.nick.lower().find(text) != -1 or \
              obj.alias.lower().find(text) != -1):
            return True
        return False
    def refilter(self):
        self.treeModelFilter.refilter()
        self.expandExpandedGroups()
    def setFilterText(self, text):
        self.filterText = text
        self.treeModelFilter.refilter()
        self.expandExpandedGroups()
    def expandExpandedGroups(self):
        '''iterate over the groups and expand the ones that have True on
        self.groupState'''
        if self.isTreeStore:
            for gRow in self.treeModelFilter:
                try:
                    obj = gRow[UserList.OBJ]
                except:
                    return
                if not (obj.id in self.groupState and
                        self.groupState[obj.id] == False) or \
                   self.orderByStatus:
                    self.expand_row(gRow.path, False)
    def sortGroupByName(self, groupDict):
        '''this method return a list of groupNames sorted according the 
        configuration'''
        l = groupDict.values() 
        l.sort( lambda x, y: cmp(x.name, y.name) )
        return l
    def sortByStatus(self, groupDict):
        '''receive a groupDict and return a dict of contacts sorted by status'''
        online = emesenelib.ContactData.Group(_('Online'))
        for group in groupDict.values():
            for user in group.users.values():
                if False: # user.status == 'FLN':
                    offline.setUser(user.email, user)
                else:
                    online.setUser(user.email, user)
        return [online]
    def getSortedContact(self, group, status , type):
        ''' return a list of contact by status and/or type '''
        contactIds = group.users.values()
        contactIds.sort( lambda x,y: self.compareContact(x, y, status, type))
        return  [x.email for x in contactIds]
    def getUserSelected(self):
        '''return the user name if a user is selected or an empty string
        if no user is selected'''
        (rowType, contact) = self.getSelected()
        if rowType == 'user':
            return contact.email
        else:
            return ''
    def groupIsSmaller(self, group1, group2):
        '''return True if the group1 < group2 according to the criteria 
        specified by the preferences'''
        return group1.name < group2.name
    def contactIsSmaller(self, contact1, contact2):
        '''return True if the contact1 < contact2 according to the criteria 
        specified by the preferences'''
        return contact1.email < contact2.email
    def allContactsOffline(self, contacts):
        '''return True if all contacts are offline'''
        return len([x for x in contacts if x.status != 'FLN']) == 0
    def add(self, values, iterator=None):
        '''add an item to the model'''
        if self.isTreeStore:
            return self.model.append(iterator, values)
        else:
            return self.model.append(values)
    def fill(self, groupDict=None, _force=False):
        '''clear and fill the user list, groupDict is a dict containing
        emesenelib.ContactData.Group objects as value and his name as a key'''
        if groupDict is None:
            groupDict = self.controller.msn.contactManager.groups
        self.set_model(None)
        self.model.clear()
        if self.smallIcons:
            self.crp.set_property('dimention', self.avatarSize / 2)
        else:
            self.crp.set_property('dimention', self.avatarSize) 
        self.userLabel = '%s\n<span size="small" foreground="' + \
            self.personalMessageColor + '">%(status)s%s</span>'
        showEmptyGroups = self.showEmptyGroups
        showOffline = self.showOffline
        if self.orderByStatus:
            sortedGroups = self.sortByStatus(groupDict)
        else:
            sortedGroups = self.sortGroupByName(groupDict)
        for group in sortedGroups:
            contacts = self.getSortedContact(group,self.sortNickGroupByStatusPriority,self.sortNickGroupByContact)
            groupIter = self.add(self.getGroupRow(group))
            offlineCount = 0
            totalCount = 0
            for contactId in contacts:
                contact = group.users[contactId]
                totalCount += 1
                if contact.status == 'FLN':
                    offlineCount +=1
                self.add(self.getContactRow(contact), groupIter)
            self.model.set_value(groupIter, UserList.TOTAL_COUNT, totalCount)
            self.model.set_value(groupIter, UserList.OFFLINE_COUNT, offlineCount)
        self.set_model(self.treeModelFilter)
        self.expandExpandedGroups()
    def getMenuData(self):
        '''Returns useful data for building menus in the format
        (<selected item type>, [[<item name>], <item's group>], <item id>)'''
        data = self.getSelected()
        if data:
            (rowType, obj) = data
            if rowType == 'user':
                return (rowType, obj.email, self.getGroupSelected().name, obj.id)
            elif rowType == 'group':
                return (rowType, obj.name, obj.id)
        else:
            return ('',)
    def getGroupLabel(self, group):
        '''return the pango string to format the group'''
        if self.showCountContact:
            return '<b>%s ( %d/%d )</b>' % (escape(group.name), \
                group.getOnlineUsersNumber(), group.getSize())
        else:
            return '<b>%s</b>' % escape(group.name)
    def getGroupRow(self, group):
        '''return the list that contain all the fields to add it to the model'''
        return [None, self.getGroupLabel(group), group.name, 'group', group, False, 0, 0, False, '']
    def getContactLabel(self, contact, showAlias=True, tooltip=False):
        '''returns a smileyrenderer list (if tooltip=False) or
        a plain pango string (if tooltip=True)'''
        status = ''
        if contact.status != 'NLN':
            index = self.controller.status_ordered[0].index(contact.status)
            status = '(' + self.unifiedParser.getParser( \
                _(self.controller.status_ordered[2][index])).get() + ')' + \
                (contact.personalMessage and ' - ')
        if contact.alias != '' and showAlias:
            label = contact.alias    
        elif self.showByNick:
            label = contact.nick
        else:
            label = contact.email
        psm = contact.personalMessage
        hasSmilies = self.parseSmilies
        smallIcons = self.smallIcons
        if smallIcons and not tooltip:
            if not psm and not status:
                template = self.userLabel.replace("\n", '')
            else:
                template = self.userLabel.replace("\n", " - ")
        else:
            template = self.userLabel
        template = template.replace("%(status)s", status)
        if tooltip:
            text = label.replace('\n', '') + '\n' + psm.replace('\n', '')
            text = self.unifiedParser.getParser(text).get().split('\n')
            try:
                label = text[0]
                psm = text[1]
            except IndexError:
                pass
            return template % (label, psm)
        else:
            tuple = (template, label, psm)
            parser = self.unifiedParser.getParser(tuple, UserListDataType)
            return parser.get(hasSmilies, self.smileysCache)
    def getContactImage(self, contact):    
        '''return the image that will be displayed on the userlist'''
        fallback = 'online'
        if self.avatarsInUserList and \
           self.theme.hasUserDisplayPicture(contact):
            return self.theme.getUserDisplayPicture(contact, \
                self.avatarSize, self.avatarSize)
        elif contact.status == 'FLN': 
            if contact.mobile:
                fallback = 'mobile'
            else:
                fallback = 'offline'
        return self.theme.getImage(fallback)
    def getContactRow(self, contact):
        '''return the list that contain all the fields to add it to the model'''
        notofflineFlag = 0 # he is offline
        if contact.status != 'FLN':
            notofflineFlag = 1 # he is not offline
        return [self.getContactImage(contact), self.getContactLabel(contact), contact.email, 'user', contact, True, notofflineFlag, 0,
                contact.blocked, contact.status]
    def getGroupIter(self, group, add=False):
        '''try to find a group and return the iter, iff not found and add == True
        add it and return the iter, if add==false and not found return None'''
        if self.isTreeStore:
            for gRow in self.treeModelFilter:
                obj = gRow[UserList.OBJ]
                if obj.name == group.name:
                    return gRow.iter
        else:
            for row in self.treeModelFilter:
                obj = row[UserList.OBJ]
                if row[UserList.TYPE] == 'group' and obj.name == group.name:
                    return row.iter
        if add:
            return self.addGroup(group)            
        return None
    def getTupleByIter(self, iterator):
        '''returns a tuple (type, object) corresponding to the given iterator
        type is a string, it can be either 'group' or 'user'
        object is a group or user instance, depending on type'''
        return (self.treeModelFilter[iterator][UserList.TYPE],
                self.treeModelFilter[iterator][UserList.OBJ])
    def getSelected(self):
        '''return a tuple containing the type ("group" or "user") and the object
        or None'''
        rows = self.get_selection().get_selected_rows()[1]
        iterator = None
        if len(rows) == 1:
            iterator = rows[0]
        if iterator is None:
            debug('invalid iter')
            return None
        return self.getTupleByIter(iterator)
    def getGroupSelected(self, iterator=None):
        '''if a group is selected return the group object, if a user is
        selected, return the group in wich its contained'''
        if iterator == None:
            iterator = self.get_selection().get_selected()[1]
        objType = self.treeModelFilter[iterator][UserList.TYPE]
        if objType == 'user':
            return self.treeModelFilter[iterator].parent[UserList.OBJ]
        elif objType == 'group':
            return self.treeModelFilter[iterator][UserList.OBJ]
    def updateGroup(self, oldGroup, group):
        '''update the values of group, we use oldGroup because the name could
        have changed, the users inside the group are not modified'''
        for row in self.model:
            obj = row[UserList.OBJ]
            if row[UserList.TYPE] == 'group' and obj.name == oldGroup.name:
                self.model.set_value(row.iter, UserList.LABEL, self.getGroupLabel(group))
                self.model.set_value(row.iter, UserList.OBJ, group)
                break
    def updateGroupNum(self,contact):
        for row in self.model:
            obj = row[UserList.OBJ]
            if row[UserList.TYPE] == 'group' and obj.getUser(contact.email) != None :
                obj.setUser(contact.email,contact)
                self.model.set_value(row.iter, UserList.LABEL, self.getGroupLabel(obj))
    def updateContact(self, contact):
        '''update the values of a contact, dont use old because the mail cant
        change'''
        if not contact:
            print 'Contact is None (UserList:703)'
            return
        doRefilter = False
        if self.isTreeStore:
            for gRow in self.model:
                for row in gRow.iterchildren():
                    if row[UserList.TYPE] == 'user' and \
                       row[UserList.OBJ].email == contact.email:
                        doRefilter = self._updateContactRow(contact, row, gRow)
        else:
            for row in self.model:
                obj = row[UserList.OBJ]
                if row[UserList.TYPE] == 'user' and obj.email == contact.email:
                    oldStatus = obj.status
                    doRefilter = self._updateContactRow(contact, row)
        if doRefilter:
            if False: #self.orderByStatus:
                self.fill(self.controller.msn.contactManager.groups)
            else:
                self.refilter()
        self.updateGroupNum(contact)
    def _updateContactRow(self, contact, row, gRow=None):
        '''updates a userlist row with new data, returns
        bool that determines if refilter is needed or not'''
        row[UserList.PIX] = self.getContactImage(contact)
        row[UserList.LABEL] = self.getContactLabel(contact)
        row[UserList.OBJ] = contact
        row[UserList.BLOCKED] = contact.blocked
        row[UserList.STATUS] = contact.status
        if gRow == None:
            return False
        if contact.status == 'FLN' and row[UserList.NOTOFFLINE_FLAG] == 1:
            row[UserList.NOTOFFLINE_FLAG] = 0
            self.model.set_value(gRow.iter, UserList.OFFLINE_COUNT, \
                gRow[UserList.OFFLINE_COUNT] + 1)
            online = gRow[UserList.TOTAL_COUNT] - gRow[UserList.OFFLINE_COUNT]
            if online == 0 or self.orderByStatus:
                return True
        elif contact.status != 'FLN' and row[UserList.NOTOFFLINE_FLAG] == 0:
            row[UserList.NOTOFFLINE_FLAG] = 1
            self.model.set_value(gRow.iter, UserList.OFFLINE_COUNT, \
                gRow[UserList.OFFLINE_COUNT] - 1)
            online = gRow[UserList.TOTAL_COUNT] - gRow[UserList.OFFLINE_COUNT]
            if online == 1 or self.orderByStatus:
                return True
        return False
    def addGroup(self, group):
        '''add a group to the list, return the gtk.TreeIter'''
        for row in self.treeModelFilter:
            obj = row[UserList.OBJ]
            if row[UserList.TYPE] == 'group' and self.groupIsSmaller(group, obj):
                if self.isTreeStore:
                    return self.model.insert_before(None, row.iter, self.getGroupRow(group))
                else:
                    return self.model.insert_before(row.iter, self.getGroupRow(group))
        if self.isTreeStore:
            return self.model.append(None, self.getGroupRow(group))
        else:
            return self.model.append(self.getGroupRow(group))
    def addContact(self, group, contact):
        '''add a contact to the given group, if this is not on the list, its added'''
        gRow = self.treeModelFilter[self.getGroupIter(group, True)]
        if self.isTreeStore:
            for row in gRow.iterchildren():
                obj = row[UserList.OBJ]
                if row[UserList.TYPE] == 'user' and self.contactIsSmaller(contact, obj):
                    return self.model.insert_before(gRow.iter, row.iter, self.getContactRow(contact))
        else:
            treeModelRow = self.treeModelFilter[gRow.iter]
            treeModelRow = treeModelRow.next
            row = self.treeModelFilter[treeModelRow.iter]
            while treeModelRow:
                row = self.treeModelFilter[treeModelRow.iter]
                obj = row[UserList.OBJ]
                if row[UserList.TYPE] == 'group':
                    break         
                if row[UserList.TYPE] == 'user' and self.contactIsSmaller(contact, obj):
                    break
                treeModelRow = treeModelRow.next
            return self.model.insert_before(row.iter, self.getContactRow(contact))
    def onRowActivated(self, treeview, path, view_column):
        tup = self.getSelected()
        objType = obj = None
        if tup:
            objType, obj = tup
        self.emit('item-selected', objType, obj, path)
    def onButtonPress(self, treeview, event):
        if event.button == 3 and self.isTreeStore:
            paths = treeview.get_path_at_pos(int(event.x), int(event.y))
            if paths == None:
                debug('invalid path')
            elif len(paths) > 0:
                iterator = self.treeModelFilter.get_iter(paths[0])
                (rowType,obj) = self.getTupleByIter(iterator)
                self.tooltips.hideTooltip()
                if rowType == 'user':
                    menu = UserMenu.UserMenu(self.controller, obj, \
                        self.getGroupSelected(iterator))
                    menu.popup(None, None, None, event.button, event.time)
                elif rowType == 'group' and not self.orderByStatus:
                    menu = GroupMenu.GroupMenu(self.controller , obj)
                    menu.popup(None, None, None, event.button, event.time)
            else:
                debug('empty paths?')
    def onRowExpanded(self,treeview, iterator, path):
        obj = self.treeModelFilter[path][UserList.OBJ]
        self.groupState[obj.id] = True
        self.saveGroupState()
    def onRowCollapsed(self,treeview, iterator, path):
        obj = self.treeModelFilter[path][4]
        self.groupState[obj.id] = False
        self.saveGroupState()
    def restoreGroupState(self):
        '''restore self.groupState'''
        value = self.config.user['collapsedGroups']
        collapsed = value.split(',')
        for gid in collapsed:
            self.groupState[gid] = False
    def saveGroupState(self):
        '''save ids of collapsed groups'''
        collapsed = []
        for gid in self.groupState:
            if not self.groupState[gid]:
                collapsed.append(gid)
        self.config.user['collapsedGroups'] = ','.join(collapsed)
gobject.type_register(UserList)
