package de.uni_passau.fim.pkjab.model.xmpp;

import java.io.IOException;
import java.util.Collections;
import java.util.Set;
import java.util.TreeSet;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import de.uni_passau.fim.pkjab.model.RestartStreamException;
import de.uni_passau.fim.pkjab.model.ConnectionCallback;
import de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import de.uni_passau.fim.pkjab.model.tags.XMLTag;
import de.uni_passau.fim.pkjab.util.Base64;
import de.uni_passau.fim.pkjab.util.Stack;

public class XMPPReaderLogin extends XMPPReaderTopLevel {
	
	//private static final String XMPP_TLS_URI = "urn:ietf:params:xml:ns:xmpp-tls";
	private static final String XMPP_SASL_URI = "urn:ietf:params:xml:ns:xmpp-sasl";
	
	private static final XMLTag SASL_MECHANISMS_TAG = new XMLTag("mechanisms", XMPP_SASL_URI);
	
	private XMPPAuthHandler auth = null;
	private Set saslMechanisms = null; // <String>
		
	public XMPPReaderLogin(final ConnectionCallback connection) {
		super(connection);
	}


	protected boolean startChild(Stack xmlStack, AbstractXMLTag thisTag,
			Attributes atts) throws SAXException {	
		boolean result = super.startChild(xmlStack, thisTag, atts);
		
		if (thisTag.uri.equals(XMPP_SASL_URI)) {
			// ignore SASL challenge (challenge, aborted, failure, success)
			// they are handled in endElement()
			return true;
		
		} else if (thisTag.equals(STREAM_ERROR_TAG)) {
			/* TODO: handle non-recoverable stream errors (RFC 3920 4.7.3) */
			XMPPReaderAdapter newHandler = new XMPPReaderIgnore(this, null);
			connection.setXMPPReader(newHandler);
			return newHandler.startElement(xmlStack, thisTag, null, atts);
			
		}
		return result;
	}


	protected boolean startGrandchild(Stack xmlStack,
			AbstractXMLTag thisTag,	Attributes atts) throws SAXException {
		boolean result = super.startGrandchild(xmlStack, thisTag, atts);
		if (xmlStack.get(1).equals(FEATURES_TAG) && thisTag.equals(SASL_MECHANISMS_TAG)) {
			saslMechanisms = new TreeSet();
			return true;
		}
		return result;
	}


	protected boolean startFurtherChild(Stack xmlStack, AbstractXMLTag thisTag,
			AbstractXMLTag lastTag, Attributes atts) throws SAXException {
		boolean result = super.startFurtherChild(xmlStack, thisTag, lastTag, atts);
		if (lastTag.equals(SASL_MECHANISMS_TAG)) {
			// ignore, mechanisms are handled in endElement()
			return true;
		}		
		return result;
	}



	protected boolean endChild(Stack xmlStack, AbstractXMLTag thisTag,
			String content) throws IOException, SAXException {
		//boolean result = super.endChild(xmlStack, thisTag, content);
		
		if (thisTag.equals(FEATURES_TAG)) {
			auth = connection.firstInitWasSuccessful(getFeatures(), saslMechanisms);
			return true;
			
		} else if (thisTag.uri.equals(XMPP_SASL_URI) && (auth != null)) {
			// handle SASL tags
			if (thisTag.getName().equals("challenge")) {
				auth.challenge(
					content != null ? Base64.decode(content) : null);
				return true;

			} else if (thisTag.getName().equals("success")) {
				auth.successful(
					content != null ? Base64.decode(content) : null);
				throw new RestartStreamException();
			}
			// TODO: aborted, failure
		}
		return false;
	}


	protected boolean endGrandchild(Stack xmlStack,
			AbstractXMLTag thisTag,	String content)
			throws IOException, SAXException {
		boolean result = super.endGrandchild(xmlStack, thisTag, content);
		
		if (xmlStack.get(1).equals(FEATURES_TAG) && thisTag.equals(SASL_MECHANISMS_TAG)) {
			saslMechanisms = Collections.unmodifiableSet(saslMechanisms);
			return true;
		}
		
		return result;
	}


	protected boolean endFurtherChild(Stack xmlStack, AbstractXMLTag thisTag,
			AbstractXMLTag lastTag, String content) throws IOException, SAXException {
		boolean result = super.endFurtherChild(xmlStack, thisTag, lastTag, content);
		
		if (lastTag.uri.equals(XMPP_SASL_URI)) {
			if (thisTag.getName().equals("mechanism")) {
				if (content != null) {
					saslMechanisms.add(content);
				} else {
					// this is not fatal, just log
					System.err.println("Empty SASL mechanism tag");
				}
				return true;
			}
		}
		
		return result;
	}
}
