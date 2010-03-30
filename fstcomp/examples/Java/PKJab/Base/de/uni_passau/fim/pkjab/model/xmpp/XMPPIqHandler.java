package de.uni_passau.fim.pkjab.model.xmpp;

import java.io.IOException;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import de.uni_passau.fim.pkjab.model.tags.Iq;
import de.uni_passau.fim.pkjab.model.tags.XMLTag;
import de.uni_passau.fim.pkjab.util.Jid;
import de.uni_passau.fim.pkjab.util.Stack;

class XMPPIqHandler extends XMPPReaderSubLevel {

	private static final String XMPP_BIND_URI = "urn:ietf:params:xml:ns:xmpp-bind";
	//private static final String XMPP_SESSIONS_URI = "urn:ietf:params:xml:ns:xmpp-session";

	private static final XMLTag BIND_TAG = new XMLTag("bind", XMPP_BIND_URI);
	
	protected XMPPIqHandler(XMPPReaderAdapter previousHandler) {
		super(previousHandler, null);
	}

	protected boolean startTopLevel(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		
		xmlStack.pop();
		xmlStack.add(new Iq(thisTag.uri, atts));
		return true;
	}

	
	protected boolean startChild(Stack xmlStack, AbstractXMLTag thisTag,
			Attributes atts) throws SAXException {
		Iq iq = (Iq) xmlStack.get(1);
		if (thisTag.getName().equals("error")) {
			// stanza error, recoverable
			iq.setError(thisTag);
		} else {
			iq.setChild(thisTag);
		}
		return true;		
	}
	
	
	protected boolean startGrandchild(Stack xmlStack,
			AbstractXMLTag thisTag,	Attributes atts) throws SAXException {
		return true;
	}
	
	
	public boolean startFurtherChild(Stack xmlStack, AbstractXMLTag thisTag,
			AbstractXMLTag lastTag, Attributes atts) throws SAXException {
		return true;
	}
	
	
	protected boolean endTopLevel(AbstractXMLTag thisTag, String content)
			throws IOException, SAXException {
		super.endTopLevel(thisTag, content);
		connection.handleQuery((Iq) thisTag);
		return true;
	}
	
	
	protected boolean endChild(Stack xmlStack, AbstractXMLTag thisTag,
			String content) throws IOException, SAXException {
		return true;
	}
	
	
	protected boolean endGrandchild(Stack xmlStack,
			AbstractXMLTag thisTag,	String content)
			throws IOException, SAXException {		
		
		if (xmlStack.get(2).equals(BIND_TAG)
				&& thisTag.uri.equals(XMPP_BIND_URI) && thisTag.getName().equals("jid")) {
			String[] parts = content.split("/");
			Jid myJid = connection.connection.getJid();
			if (parts.length == 2) {
				if (!parts[0].equals(myJid.getBareJid())) {
					System.err.println("Server bound us to wrong user: " + parts[0]);
				}
				if (!parts[1].equals(myJid.getResource())) {
					System.out.println("Server gave us a new resource: " + parts[1]);
					connection.setResource(parts[1]);
				}
			} else {
				System.err.println("Invalid jid content: " + content);
			}
			return true;
		}
		return false;
	}
}
