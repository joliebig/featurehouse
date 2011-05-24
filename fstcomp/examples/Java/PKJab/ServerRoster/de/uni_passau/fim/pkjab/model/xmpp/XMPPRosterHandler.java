package de.uni_passau.fim.pkjab.model.xmpp;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import de.uni_passau.fim.pkjab.model.Contact;
import de.uni_passau.fim.pkjab.model.Roster;
import de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import de.uni_passau.fim.pkjab.model.tags.XMLTag;
import de.uni_passau.fim.pkjab.util.Jid;
import de.uni_passau.fim.pkjab.util.Stack;
import de.uni_passau.fim.pkjab.util.Subscription;

/**
 * Handles *content* of
 * <iq type='result'><query xmlns='jabber:iq:roster></query></iq>
 * 
 * @author Philipp Wendler
 */
class XMPPRosterHandler extends XMPPReaderSubLevel {

	private static final XMLTag ROSTER_ITEM_TAG = new XMLTag("item", ROSTER_URI);
	private static final XMLTag ROSTER_GROUP_TAG = new XMLTag("group", ROSTER_URI);
	
	protected XMPPRosterHandler(final XMPPReaderAdapter previousHandler, 
			final Stack xmlStack) {
		super(previousHandler, ROSTER_URI);
	}
	

	protected boolean startTopLevel(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		String type = atts.getValue("type"); 
		return (type == null || type.equals("result") || type.equals("set"));
	}
	

	protected boolean startChild(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) {
		if (thisTag.equals(ROSTER_ITEM_TAG)) {
			Jid jid = Jid.fromString(atts.getValue("jid"));
			
			if (jid != null) {
				final Roster roster = connection.connection.getRoster();
				Contact contact = roster.get(jid);
				if (contact == null) {
					contact = new Contact(connection.connection, jid);
					roster.add(contact);
				}
				
				contact.setNick(atts.getValue("name"));
				contact.setSubscription(Subscription.valueOf(atts.getValue("subscription")));
				contact.fireStateChanged();
				return true;
			}
		}
		
		return false;
	}


	protected boolean startGrandchild(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) {
			return thisTag.equals(ROSTER_GROUP_TAG);
	}



	protected boolean endChild(Stack xmlStack,
			AbstractXMLTag thisTag, String content) {
		return thisTag.equals(ROSTER_ITEM_TAG);
	}


	protected boolean endGrandchild(Stack xmlStack,
			AbstractXMLTag thisTag, String content) {
		return thisTag.equals(ROSTER_GROUP_TAG);
	}
}