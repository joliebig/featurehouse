'''a gtk implementation of abstract.ContactList'''
import gtk
import gobject
import abstract.ContactList
import abstract.status as status
from abstract.GroupManager import Group
from abstract.ContactManager import Contact
class ContactList(abstract.ContactList.ContactList, gtk.TreeView):
    '''a gtk implementation of abstract.ContactList'''
    def __init__(self, contacts, groups, dialog):
        '''class constructor'''
        abstract.ContactList.ContactList.__init__(self, contacts, groups, 
            dialog)
        gtk.TreeView.__init__(self)
        self._model = gtk.TreeStore(gtk.gdk.Pixbuf, object, str)
        self.model = self._model.filter_new(root=None)
        self.model.set_visible_func(self._visible_func)
        self._model.set_sort_func(1, self._sort_method)
        self._model.set_sort_column_id(1, gtk.SORT_ASCENDING)
        self.set_model(self.model)
        crt = gtk.CellRendererText()
        column = gtk.TreeViewColumn()
        column.set_expand(True)
        exp_column = gtk.TreeViewColumn()
        exp_column.set_max_width(16)       
        self.append_column(exp_column)
        self.append_column(column)
        self.set_expander_column(exp_column)
        column.pack_start(crt)
        column.add_attribute(crt, 'markup', 2)
        self.set_search_column(2)
        self.set_headers_visible(False)
        self.connect('row-activated', self._on_row_activated)
        self.connect('button-press-event' , self._on_button_press_event)
        self.nick_template = '%DISPLAY_NAME%\n'
        self.nick_template += '<span foreground="#AAAAAA" size="small">'
        self.nick_template += '%ACCOUNT%\n(%STATUS%) - %MESSAGE%</span>'
        self.group_template = '<b>%NAME% (%ONLINE_COUNT%/%TOTAL_COUNT%)</b>'
    def _visible_func(self, model, _iter):
        '''return True if the row should be displayed according to the 
        value of the config'''
        obj = self._model[_iter][1]
        if not obj:
            return
        if type(obj) == Group:
            if not self.show_empty_groups:
                contacts = self.contacts.get_contacts(obj.contacts)
                if  self.contacts.get_online_total_count(contacts)[0] == 0:
                    return False
            return True
        if self._filter_text:
            if '\n'.join((obj.account, obj.alias, obj.nick, obj.message, 
                obj.account)).lower().find(self._filter_text) == -1:
                return False
            else:
                return True
        if not self.show_offline and obj.status == status.OFFLINE:
            return False
        return True
    def _sort_method(self, model, iter1, iter2, user_data=None):
        '''callback called to decide the order of the contacts'''
        obj1 = self._model[iter1][1]
        obj2 = self._model[iter2][1]
        if type(obj1) == Group and type(obj2) == Group:
            return self.compare_groups(obj1, obj2)
        elif type(obj1) == Contact and type(obj2) == Contact:
            return self.compare_contacts(obj1, obj2)
        elif type(obj1) == Group and type(obj2) == Contact:
            return -1
        else:
            return 1
    def _get_selected(self):
        '''return the selected row or None'''
        return self.model.convert_iter_to_child_iter(\
            self.get_selection().get_selected()[1])
    def _on_row_activated(self, treeview, path, view_column):
        '''callback called when the user selects a row'''
        group = self.get_group_selected()
        contact = self.get_contact_selected()
        if group:
            self.signal_emit('group-selected', group)
        elif contact:
            self.signal_emit('contact-selected', contact)
        else:
            print 'nothing selected?'
    def _on_button_press_event(self, treeview, event):
        '''callback called when the user press a button over a row
        chek if it's the roght button and emit a signal on that case'''
        if event.button == 3:
            paths = self.get_path_at_pos(int(event.x), int(event.y))
            if paths is None:
                print 'invalid path'
            elif len(paths) > 0:
                iterator = self.model.get_iter(paths[0])
                child_iter = self.model.convert_iter_to_child_iter(iterator)
                obj = self._model[child_iter][1]
                if type(obj) == Group:
                    self.signal_emit('group-menu-selected', obj)
                elif type(obj) == Contact:
                    self.signal_emit('contact-menu-selected', obj)
            else:
                print 'empty paths?'
    def refilter(self):
        '''refilter the values according to the value of self.filter_text'''
        self.model.refilter()
    def is_group_selected(self):
        '''return True if a group is selected'''
        return type(self._model[self._get_selected()][1]) == Group
    def is_contact_selected(self):
        '''return True if a contact is selected'''
        return type(self._model[self._get_selected()][1]) == Contact
    def get_group_selected(self):
        '''return a group object if there is a group selected, None otherwise
        '''
        if self.is_group_selected():
            return self._model[self._get_selected()][1]
        return None
    def get_contact_selected(self):
        '''return a contact object if there is a group selected, None otherwise
        '''
        if self.is_contact_selected():
            return self._model[self._get_selected()][1]
        return None
    def add_group(self, group):
        '''add a group to the contact list'''
        if self.order_by_status:
            return None
        group_data = (None, group, self.format_group(group))
        for row in self._model:
            obj = row[1]
            if type(obj) == Group:
                if obj.name == group.name:
                    print 'Trying to add an existing group!', obj.name
                    return row.iter
        return self._model.append(None, group_data)
    def remove_group(self, group):
        '''remove a group from the contact list'''
        for row in self._model:
            obj = row[1]
            if type(obj) == Group and obj.name == group.name:
                del self._model[row.iter]
    def add_contact(self, contact, group=None):
        '''add a contact to the contact list, add it to the group if 
        group is not None'''
        contact_data = (None, contact, self.format_nick(contact))
        if not group or self.order_by_status:
            for row in self._model:
                obj = row[1]
                if type(obj) == Group:
                    for contact_row in row.iterchildren():
                        con = contact_row[1]
                        if con.account == contact.account:
                            return contact_row.iter
                elif type(obj) == Contact and obj.account == contact.account:
                    return row.iter
            return self._model.append(None, contact_data) 
        for row in self._model:
            obj = row[1]
            if type(obj) == Group and obj.name == group.name:
                return_iter = self._model.append(row.iter, contact_data)
                self.update_group(group)
                for irow in self._model:
                    iobj = irow[1]
                    if type(iobj) == Contact and \
                            iobj.account == contact.account:
                        del self._model[irow.iter]
                return return_iter
        else:            
            self.add_group(group)
            return self.add_contact(contact, group)
    def remove_contact(self, contact, group=None):
        '''remove a contact from the specified group, if group is None
        then remove him from all groups'''
        if not group:
            for row in self._model:
                obj = row[1]
                if type(obj) == Group:
                    for contact_row in row.iterchildren():
                        con = contact_row[1]
                        if con.account == contact.account:
                            del self._model[contact_row.iter]
                            self.update_group(obj)
                elif type(obj) == Contact and obj.account == contact.account:
                    del self._model[row.iter]
            return
        for row in self._model:
            obj = row[1]
            if type(obj) == Group and obj.name == group.name:
                for contact_row in row.iterchildren():
                    con = contact_row[1]
                    if con.account == contact.account:
                        del self._model[contact_row.iter]
                        self.update_group(group)
    def clear(self):
        '''clear the contact list'''
        self._model.clear()
    def update_contact(self, contact):
        '''update the data of contact'''
        contact_data = (None, contact, self.format_nick(contact))
        for row in self._model:
            obj = row[1]
            if type(obj) == Group:
                for contact_row in row.iterchildren():
                    con = contact_row[1]
                    if con.account == contact.account:
                        self._model[contact_row.iter] = contact_data
                        self.update_group(obj)
            elif type(obj) == Contact and obj.account == contact.account:
                self._model[row.iter] = contact_data
    def update_group(self, group):
        '''update the data of group'''
        group_data = (None, group, self.format_group(group))
        for row in self._model:
            obj = row[1]
            if type(obj) == Group and obj.name == group.name:
                self._model[row.iter] = group_data
    def set_group_state(self, group, state):
        '''expand group id state is True, collapse it if False'''
        for row in self._model:
            obj = row[1]
            if type(obj) == Group and obj.name == group.name:
                path = self._model.get_path()
                if state:
                    self.expand_row(path, False)
                else:
                    self.collapse_row(path)
    def format_nick(self, contact):
        '''replace the appearance of the template vars using the values of
        the contact
        '''
        template = self.nick_template
        template = template.replace('%NICK%', 
            gobject.markup_escape_text(contact.nick))
        template = template.replace('%ACCOUNT%',
            gobject.markup_escape_text(contact.account))
        template = template.replace('%MESSAGE%', 
            gobject.markup_escape_text(contact.message))
        template = template.replace('%STATUS%', 
            gobject.markup_escape_text(status.STATUS[contact.status]))
        template = template.replace('%DISPLAY_NAME%', 
            gobject.markup_escape_text(contact.display_name))
        return template
    def format_group(self, group):
        '''replace the appearance of the template vars using the values of
        the group
        '''
        contacts = self.contacts.get_contacts(group.contacts)
        (online, total) = self.contacts.get_online_total_count(contacts)       
        template = self.group_template
        template = template.replace('%NAME%', 
            gobject.markup_escape_text(group.name))
        template = template.replace('%ONLINE_COUNT%', str(online))
        template = template.replace('%TOTAL_COUNT%', str(total))
        return template
def test():
    import dialog
    import string
    import random
    import ContactManager
    import GroupManager
    def _on_contact_selected(contact_list, contact):
        '''callback for the contact-selected signal'''
        print 'contact selected: ', contact.display_name
        contact.nick = random_string()
        contact.status = random.choice(status.ORDERED)
        contact.message = random_string()
        contact_list.update_contact(contact)
    def _on_group_selected(contact_list, group):
        '''callback for the group-selected signal'''
        print 'group selected: ', group.name
        group.name = random_string()
        contact_list.update_group(group)
    def _on_contact_menu_selected(contact_list, contact):
        '''callback for the contact-menu-selected signal'''
        print 'contact menu selected: ', contact.display_name
        contact_list.remove_contact(contact)
    def _on_group_menu_selected(contact_list, group):
        '''callback for the group-menu-selected signal'''
        print 'group menu selected: ', group.name
        contact_list.remove_group(group)
    def random_string(length=6):
        '''generate a random string of length "length"'''
        return ''.join([random.choice(string.ascii_letters) for i \
            in range(length)])
    def random_mail():
        '''generate a random mail'''
        return random_string() + '@' + random_string() + '.com'
    contactm = ContactManager.ContactManager(dialog, None, 'msn@msn.com')
    groupm = GroupManager.GroupManager(dialog, None)
    window = gtk.Window()
    window.set_default_size(200, 200)
    scroll = gtk.ScrolledWindow()
    conlist = ContactList(contactm, groupm, dialog)
    conlist.signal_connect('contact-selected', _on_contact_selected)
    conlist.signal_connect('contact-menu-selected', _on_contact_menu_selected)
    conlist.signal_connect('group-selected', _on_group_selected)
    conlist.signal_connect('group-menu-selected', _on_group_menu_selected)
    conlist.order_by_status = True
    conlist.show_offline = True
    scroll.add(conlist)
    window.add(scroll)
    window.connect("delete-event", gtk.main_quit)
    window.show_all()
    for i in range(6):
        group = Group(random_string())
        groupm.register(group)
        for j in range(6):
            contact = Contact(random_mail(), i * 10 + j, random_string())
            contact.status = random.choice(status.ORDERED)
            contact.message = random_string()
            group.contacts.append(contact.account)
            contactm.register(contact)
            conlist.add_contact(contact, group)
    for j in range(6):
        contact = Contact(random_mail(), 100 + j, random_string())
        contact.status = random.choice(status.ORDERED)
        contact.message = random_string()
        contactm.register(contact)
        conlist.add_contact(contact)
    gtk.main()
if __name__ == '__main__':
    test()
