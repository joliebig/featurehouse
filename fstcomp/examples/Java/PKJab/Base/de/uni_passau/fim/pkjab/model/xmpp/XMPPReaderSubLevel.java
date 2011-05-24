package de.uni_passau.fim.pkjab.model.xmpp;

import java.io.IOException;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import de.uni_passau.fim.pkjab.util.Stack;

abstract class XMPPReaderSubLevel extends XMPPReaderAdapter {

	protected static final String ROSTER_URI = "jabber:iq:roster";
	
	private final XMPPReaderAdapter previousHandler;
	
	protected XMPPReaderSubLevel(final XMPPReaderAdapter previousHandler,
			final String namespace) {
		super(previousHandler.connection, previousHandler.xmlStack, namespace);
		this.previousHandler = previousHandler;
	}

	protected void returnControl() {
		connection.setXMPPReader(previousHandler);
	}
	

	protected boolean startTopLevel(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		// believe the creating Handler that this is our tag
		return true;
	}
	

	protected boolean startChild(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		return false;
	}


	protected boolean startGrandchild(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		return false;
	}


	protected boolean startFurtherChild(Stack xmlStack,
			AbstractXMLTag thisTag, AbstractXMLTag lastTag, Attributes atts)
			throws SAXException {
		return false;
	}
	

	/**
	 * If overwritten, either call super or returnControl!
	 */

	protected boolean endTopLevel(AbstractXMLTag thisTag, String content)
			throws IOException, SAXException {
		returnControl();
		return true;
	}
	

	protected boolean endChild(Stack xmlStack,
			AbstractXMLTag thisTag, String content) throws IOException,
			SAXException {
		return false;
	}


	protected boolean endGrandchild(Stack xmlStack,
			AbstractXMLTag thisTag, String content)
			throws IOException, SAXException {
		return false;
	}


	protected boolean endFurtherChild(Stack xmlStack,
			AbstractXMLTag thisTag, AbstractXMLTag lastTag, String content)
			throws IOException, SAXException {
		return false;
	}	
}
