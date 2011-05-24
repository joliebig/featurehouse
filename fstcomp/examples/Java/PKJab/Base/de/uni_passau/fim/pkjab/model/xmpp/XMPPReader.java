package de.uni_passau.fim.pkjab.model.xmpp;

import java.io.IOException;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import de.uni_passau.fim.pkjab.model.ConnectionCallback;
import de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import de.uni_passau.fim.pkjab.model.tags.XMLTag;
import de.uni_passau.fim.pkjab.util.Stack;

public class XMPPReader extends XMPPReaderTopLevel {
	
	//private static final String XMPP_STREAMS_URI = "urn:ietf:params:xml:ns:xmpp-streams";
	
	private static final XMLTag PRESENCE_TAG = new XMLTag("presence", JABBER_URI);
	private static final XMLTag IQ_TAG = new XMLTag("iq", JABBER_URI);
	private static final XMLTag MESSAGE_TAG = new XMLTag("message", JABBER_URI);

	private String lang = null;
	
	public XMPPReader(final ConnectionCallback connection) {
		super(connection);
	}
	
	public String getLang() {
		return lang;
	}
	
	protected void startStream(XMLTag thisTag, Attributes atts) {
		lang = atts.getValue("lang");
	}


	protected boolean startChild(Stack xmlStack, AbstractXMLTag thisTag,
			Attributes atts) throws SAXException {
		boolean result = super.startChild(xmlStack, thisTag, atts); 
		
		XMPPReaderAdapter newHandler = null;
		
		if (thisTag.equals(IQ_TAG)) {
			newHandler = new XMPPIqHandler(this);

		} else if (thisTag.equals(STREAM_ERROR_TAG)) {
			/* TODO: handle non-recoverable stream errors (RFC 3920 4.7.3) */
			newHandler = new XMPPReaderIgnore(this, null);
			
		} else if (thisTag.equals(PRESENCE_TAG)) {
			newHandler = new XMPPPresenceHandler(this);
			
		} else if (thisTag.equals(MESSAGE_TAG)) {
			newHandler = new XMPPMessageHandler(this);
		}
		
		if (newHandler != null) {
			connection.setXMPPReader(newHandler);
			return newHandler.startElement(xmlStack, thisTag, null, atts);
		} else { 
			return result;
		}
	}


	protected boolean endChild(Stack xmlStack, AbstractXMLTag thisTag,
			String content) throws IOException, SAXException {
		if (thisTag.equals(FEATURES_TAG)) {
			connection.secondInitWasSuccessful(getFeatures());
			return true;	
		}
		return false;
	}
}
