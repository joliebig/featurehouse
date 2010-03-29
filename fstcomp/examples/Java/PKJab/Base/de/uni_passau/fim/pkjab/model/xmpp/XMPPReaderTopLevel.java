
import java.io.IOException;
import java.util.Collections;
import java.util.Set;
import java.util.TreeSet;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import pkjab.de.uni_passau.fim.pkjab.model.ConnectionCallback;
import pkjab.de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import pkjab.de.uni_passau.fim.pkjab.model.tags.XMLTag;
import pkjab.de.uni_passau.fim.pkjab.util.Stack;

abstract class XMPPReaderTopLevel extends XMPPReaderAdapter {

	protected static final String STREAMS_URI = "http://etherx.jabber.org/streams";

	protected static final XMLTag FEATURES_TAG = new XMLTag("features", STREAMS_URI);
	protected static final XMLTag STREAM_TAG = new XMLTag("stream", STREAMS_URI);
	protected static final XMLTag STREAM_ERROR_TAG = new XMLTag("error", STREAMS_URI);

	private Set features = null; // <AbstractXMLTag>
	
	protected XMPPReaderTopLevel(final ConnectionCallback connection) {
		super(connection, new Stack(), null);
	}
		
	protected Set getFeatures() {
		return features != null ? Collections.unmodifiableSet(features) : null;
	}
	
	protected void startStream(AbstractXMLTag thisTag, Attributes atts) { }
	

	protected boolean startTopLevel(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		// handle top level stream element
		if (thisTag.equals(STREAM_TAG)) {
			String serverDomain = atts.getValue("", "from");
			String expectedDomain = connection.connection.getJid().getDomain();
			if (!expectedDomain.equals(serverDomain)) {
				System.err.println("Domains differ: "
					+ expectedDomain + " and " + serverDomain);
			}
			startStream(thisTag, atts);
			return true;
		}
		return false;
	}
	

	protected boolean startChild(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		if (thisTag.equals(FEATURES_TAG)) {
			features = new TreeSet();
			return true;
		}
		return false;
	}


	protected boolean startGrandchild(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		if (xmlStack.get(1).equals(FEATURES_TAG)) {
			features.add(thisTag);
			return true;
		}
		return false;
	}
	

	protected boolean startFurtherChild(Stack xmlStack,
			AbstractXMLTag thisTag, AbstractXMLTag lastTag, Attributes atts) 
	 		throws SAXException {
		if (xmlStack.get(1).equals(FEATURES_TAG)) {
			return true;
		}
		return false;
	}
	
	

	protected boolean endTopLevel(AbstractXMLTag thisTag, String content)
			throws IOException, SAXException {
		 // stream is closed? WTF?
		throw new SAXException("Server closed stream tag!");
	}
	

	protected boolean endChild(Stack xmlStack,
			AbstractXMLTag thisTag, String content) throws IOException, SAXException {
		if (thisTag.equals(FEATURES_TAG)) {
			return true;
		}
		return false;
	}


	protected boolean endGrandchild(Stack xmlStack,
			AbstractXMLTag thisTag, String content)
			throws IOException, SAXException {
		if (xmlStack.get(1).equals(FEATURES_TAG)) {
			// ignore all feature tags
			return true;
		}
		return false;
	}


	protected boolean endFurtherChild(Stack xmlStack,
			AbstractXMLTag thisTag, AbstractXMLTag lastTag, String content)
			throws IOException, SAXException {
		// ignore all further children in features
		if (xmlStack.get(1).equals(FEATURES_TAG)) {
			return true;
		}
		return false;
	}
}
