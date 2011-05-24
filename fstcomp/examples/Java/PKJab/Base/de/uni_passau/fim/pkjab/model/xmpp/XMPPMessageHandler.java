package de.uni_passau.fim.pkjab.model.xmpp;

import java.io.IOException;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import de.uni_passau.fim.pkjab.model.Contact;
import de.uni_passau.fim.pkjab.model.Roster;
import de.uni_passau.fim.pkjab.model.messages.TextMessage;
import de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import de.uni_passau.fim.pkjab.model.tags.XMLTag;
import de.uni_passau.fim.pkjab.util.Jid;
import de.uni_passau.fim.pkjab.util.Stack;

class XMPPMessageHandler extends XMPPReaderIgnore {

	private static XMLTag SUBJECT_TAG = new XMLTag("subject", JABBER_URI); 
	private static XMLTag BODY_TAG = new XMLTag("body", JABBER_URI); 
	private static XMLTag THREAD_TAG = new XMLTag("thread", JABBER_URI); 
		
	protected Contact contact = null;
	protected TextMessage message = new TextMessage();

	protected XMPPMessageHandler(final XMPPReaderAdapter previousHandler) {
		super(previousHandler, null);
	}

	
	protected boolean startTopLevel(Stack xmlStack, 
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		Jid to = Jid.fromString(atts.getValue("to"));
		Jid myJid = connection.connection.getJid();
		Jid from = Jid.fromString(atts.getValue("from"));
		if (from != null && from.isQualified()
				&& (to == null || to.equals(myJid) || to.equals(myJid.getBareJid()))) {
			Roster roster = connection.connection.getRoster();
			contact = roster.get(from);
			if (contact == null) {
				contact = new Contact(connection.connection, from);
				roster.add(contact);
			}
			message.setType(atts.getValue("type"));
			message.setTo(to);
			message.setFrom(from);
			
			return true;
		} else {
			return false;
		}
	}
	
	
	protected boolean startChild(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		if (thisTag.uri.equals(JABBER_URI)) {
			return thisTag.equals(BODY_TAG) || thisTag.equals(THREAD_TAG)
				|| thisTag.equals(SUBJECT_TAG);
		} else {
			XMPPReaderAdapter newHandler = new XMPPReaderIgnore(this, null);
			connection.setXMPPReader(newHandler);
			return newHandler.startElement(xmlStack, thisTag, null, atts);
		}			
	}
	
	
	protected boolean endTopLevel(AbstractXMLTag thisTag, String content)
		throws IOException, SAXException {
		super.endTopLevel(thisTag, content);
		if (message.getBody() != null) {
			contact.setMessageReceived(message);
		}
		return true;
	}

	
	protected boolean endChild(Stack xmlStack,
			AbstractXMLTag thisTag, String content) {
		if (thisTag.equals(SUBJECT_TAG)) {
			message.setSubject(content);
			return true;
			
		} else if (thisTag.equals(BODY_TAG)) {
			message.setBody(content);
			return true;
			 
		} else if (thisTag.equals(THREAD_TAG)) {
			message.setThread(content);
			return true;
		
		}
		return false;
	}
}