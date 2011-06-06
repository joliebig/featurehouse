from emesenelib import soap, common
def membership(callback, sync):
    common.debug("soap.requests: membership list", "soap")
    soap.manager.do_request(
        'http://www.msn.com/webservices/AddressBook/FindMembership',
        'contacts.msn.com', 443, '/abservice/SharingService.asmx',
        soap.templates.membershipList, callback, sync=sync)
def address_book(callback, sync):
    common.debug("soap.requests: address book", "soap")
    soap.manager.do_request(
        'http://www.msn.com/webservices/AddressBook/ABFindAll',
        'contacts.msn.com', 443, '/abservice/abservice.asmx',
        soap.templates.addressBook, callback, sync=sync)
def dynamic_items(callback, sync):
    common.debug("soap.requests: dynamic items", "soap")
    soap.manager.do_request(
        'http://www.msn.com/webservices/AddressBook/ABFindAll',
        'contacts.msn.com', 443, '/abservice/abservice.asmx',
        soap.templates.dynamicItems, callback, sync=sync)
def schematized_store(email, callback, *args):
    soap.manager.do_request(
        'http://www.msn.com/webservices/storage/w10/GetItemVersion',
        'storage.msn.com', 80, '/storageservice/schematizedstore.asmx',
        soap.templates.schematizedStore%(email), callback, args)
def change_nick(nick, callback, *args):
    soap.manager.do_request(
        'http://www.msn.com/webservices/AddressBook/ABContactUpdate',
        'omega.contacts.msn.com', 443, '/abservice/abservice.asmx',
        soap.templates.changeNick % ('Me', common.escape(nick)),
        callback, args)
def change_alias(contactID, alias, callback, *args):
    alias = str(common.escape(alias))
    soap.manager.do_request(
        'http://www.msn.com/webservices/AddressBook/ABContactUpdate',
        'omega.contacts.msn.com', 443, '/abservice/abservice.asmx',
        soap.templates.renameContact % (str(contactID), alias),
        callback, args)
def add_contact(email, callback, *args):
    soap.manager.do_request(
        'http://www.msn.com/webservices/AddressBook/ABContactAdd',
        'omega.contacts.msn.com', 443, '/abservice/abservice.asmx',
        soap.templates.contactAdd % (email, ), callback, args)
def remove_contact(contactID, callback, *args):
    soap.manager.do_request(
        'http://www.msn.com/webservices/AddressBook/ABContactDelete',
        'omega.contacts.msn.com', 443, '/abservice/abservice.asmx',
        soap.templates.contactRemove % (contactID, ), callback, args)
def add_to_group(gid, contactID, callback, *args):
    soap.manager.do_request(
        'http://www.msn.com/webservices/AddressBook/ABGroupContactAdd',
        'omega.contacts.msn.com', 443, '/abservice/abservice.asmx',
        soap.templates.moveUserToGroup % (gid, contactID), callback, args)
def remove_from_group(contactID, sourceGid, callback, *args):
    soap.manager.do_request(
        'http://www.msn.com/webservices/AddressBook/ABGroupContactDelete',
        'omega.contacts.msn.com', 443, '/abservice/abservice.asmx',
        soap.templates.deleteUserFromGroup % (contactID, sourceGid),
        callback, args)
def add_role(role, email, callback, *args):
    soap.manager.do_request(
        'http://www.msn.com/webservices/AddressBook/AddMember',
        'omega.contacts.msn.com', 443, '/abservice/SharingService.asmx',
        soap.templates.addMember % (role, email), callback, args)
def delete_role(role, email, callback, *args):
    soap.manager.do_request(
        'http://www.msn.com/webservices/AddressBook/DeleteMember',
        'omega.contacts.msn.com', 443, '/abservice/SharingService.asmx',
        soap.templates.deleteMember % (role, email), callback, args)
