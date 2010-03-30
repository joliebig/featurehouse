package de.uni_passau.fim.pkjab.model;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

import de.uni_passau.fim.pkjab.util.Jid;

public class Roster {
	
	private Map contacts = new TreeMap(); // <Jid, Contact>

	Roster() {
		
	}
	
	public void add(Contact contact) {
		contacts.put(contact.getBareJid(), contact);
		contact.connection.notifyObservers(null);
	}
	
	public Contact get(Jid jid) {
		if (jid.isQualified()) {
			jid = new Jid(jid.getUser(), jid.getDomain(), null);
		}
		return (Contact)contacts.get(jid);
	}
	
	public Contact remove(Jid jid) {
		return (Contact)contacts.remove(jid);
	}
	
	public Collection getContacts() {
		return Collections.unmodifiableCollection(contacts.values());
	}
	
	public String toString() {
		return getContacts().toString();
	}
}
