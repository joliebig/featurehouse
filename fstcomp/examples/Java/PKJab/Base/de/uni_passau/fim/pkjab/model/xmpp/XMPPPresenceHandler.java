
import java.io.IOException;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import pkjab.de.uni_passau.fim.pkjab.model.Contact;
import pkjab.de.uni_passau.fim.pkjab.model.Resource;
import pkjab.de.uni_passau.fim.pkjab.model.messages.PresenceInformation;
import pkjab.de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import pkjab.de.uni_passau.fim.pkjab.model.tags.XMLTag;
import pkjab.de.uni_passau.fim.pkjab.util.Jid;
import pkjab.de.uni_passau.fim.pkjab.util.Stack;
import pkjab.de.uni_passau.fim.pkjab.util.UserState;

/**
 * Handles *content* of
 * <presence></presence>
 * 
 * @author Philipp Wendler
 */
class XMPPPresenceHandler extends XMPPReaderSubLevel {

	private static XMLTag PRIORITY_TAG = new XMLTag("priority", JABBER_URI); 
	private static XMLTag SHOW_TAG = new XMLTag("show", JABBER_URI); 
	private static XMLTag STATUS_TAG = new XMLTag("status", JABBER_URI); 
	
	private Contact contact = null;
	private Resource resource = null;
	
	protected XMPPPresenceHandler(final XMPPReaderAdapter previousHandler) {
		super(previousHandler, JABBER_URI);
	}
	
	
	protected boolean startTopLevel(Stack xmlStack, 
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		Jid to = Jid.fromString(atts.getValue("to"));
		Jid myJid = connection.connection.getJid();
		Jid from = Jid.fromString(atts.getValue("from"));
		String type = atts.getValue("type");
		if (from != null && (to == null || to.equals(myJid) || to.equals(myJid.getBareJid()))) {
			contact = connection.connection.getRoster().get(from);
			
			// TODO: handle error
			if (contact != null) {
				if (from.isQualified()) {
					resource = new Resource(from.getResource());
					if (type == null) {
						resource.setState(UserState.ONLINE);
					} else if (type.equals("unavailable")) {
						resource.setState(UserState.OFFLINE);
					} else {
						return false;
					}
				} else {
					PresenceInformation message = new PresenceInformation();
					message.setFrom(from);
					message.setTo(to);
					message.setType(type);
					contact.setMessageReceived(message);
					if (type.equals("subscribe")) {
						contact.setSubscription(contact.getSubscription().addTo());
					} else if (type.equals("subscribed")) {
						contact.setSubscription(contact.getSubscription().addFrom());
					} else if (type.equals("unsubscribe")) {
						contact.setSubscription(contact.getSubscription().removeTo());
					} else if (type.equals("unsubscribed")) {
						contact.setSubscription(contact.getSubscription().removeFrom());
					} else {
						return false;
					}
				}
			}

			return true;
		} else {
			return false;
		}
	}
	
	
	protected boolean startChild(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		return thisTag.equals(PRIORITY_TAG) || thisTag.equals(SHOW_TAG)
				|| thisTag.equals(STATUS_TAG);
	}

	
	protected boolean endTopLevel(AbstractXMLTag thisTag, String content)
			throws IOException, SAXException {
		super.endTopLevel(thisTag, content);
		if (resource != null) {
			contact.addResource(resource);
		}
		if (contact != null) {
			contact.fireStateChanged();
		}
		return true;
	}

	
	protected boolean endChild(Stack xmlStack,
			AbstractXMLTag thisTag, String content) {
		if (resource != null) {
			if (thisTag.equals(PRIORITY_TAG)) {
				resource.setPriority(Integer.parseInt(content.trim()));
				return true;
				
			} else if (thisTag.equals(SHOW_TAG)) {
				resource.setState(UserState.valueOf(content.trim().toUpperCase()));
				return true;
				 
			} else if (thisTag.equals(STATUS_TAG)) {
				resource.setText(content);
				return true;
			
			}
			return false;
			
		} else {
			return thisTag.equals(PRIORITY_TAG) || thisTag.equals(SHOW_TAG)
				|| thisTag.equals(STATUS_TAG);
		}
	}
}
