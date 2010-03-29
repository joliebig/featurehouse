
import java.io.IOException;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;

import pkjab.de.uni_passau.fim.pkjab.model.InvalidXMLException;
import pkjab.de.uni_passau.fim.pkjab.model.RestartStreamException;
import pkjab.de.uni_passau.fim.pkjab.model.UnknownXMLException;
import pkjab.de.uni_passau.fim.pkjab.model.ConnectionCallback;
import pkjab.de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import pkjab.de.uni_passau.fim.pkjab.model.tags.XMLTag;
import pkjab.de.uni_passau.fim.pkjab.util.Stack;

abstract class XMPPReaderAdapter implements ContentHandler {

	protected static final String JABBER_URI = "jabber:client";

	protected final ConnectionCallback connection;

	final Stack xmlStack; // <AbstractXMLTag>
	private final int offset;
	private final String namespace;
	private StringBuffer contentCharacters = null;
	
	protected XMPPReaderAdapter(final ConnectionCallback connection,
			final Stack xmlStack, final String namespace) {
		this.connection = connection;
		this.xmlStack = xmlStack;
		this.offset = Math.max(0, xmlStack.size()-1);
		this.namespace = namespace;
	}
	
	
	public void characters(char[] ch, int start, int length)
			throws SAXException {
		if (contentCharacters == null) {
			contentCharacters = new StringBuffer();
		}
		contentCharacters.append(ch, start, length);
	}

	
	public void startDocument() throws SAXException {
		xmlStack.clear();
	}	
	
	
	public void endDocument() throws SAXException {
		System.err.println("XML ended!");
		throw new RestartStreamException();
	}
	
	
	public final void startElement(String uri, String localName, String name,
			Attributes atts) throws SAXException {
		AbstractXMLTag lastTag = xmlStack.peek();
		XMLTag thisTag = new XMLTag(localName, uri);
		xmlStack.add(thisTag);
		System.out.println("startElement: " + thisTag);
		
		// switch over xml tree depth and handle tags
		// if unsuccessful, an exception is thrown
		if (!startElement(xmlStack, thisTag, lastTag, atts)) {
			// during Development, ignore unknown tags:
			System.out.println("Defaulting to ignore for unhandled tag " + thisTag);
			XMPPReaderAdapter newHandler = new XMPPReaderIgnore(this, null);
			connection.setXMPPReader(newHandler);
			newHandler.startElement(xmlStack, thisTag, null, atts);		

//			throw new UnknownXMLException(xmlStack);
		}
	}
	
	protected final boolean startElement(Stack xmlStack,
			AbstractXMLTag thisTag, AbstractXMLTag lastTag, Attributes atts)
			throws SAXException {
		boolean result;
		int oldStackSize = xmlStack.size();
		
		if ((namespace == null) || namespace.equals(thisTag.uri) || (thisTag.uri == null)) {
			switch (oldStackSize-offset) {
			case 1:
				result = startTopLevel(xmlStack, thisTag, atts);
				break;
				
			case 2:
				result = startChild(xmlStack, thisTag, atts);
				break;
				
			case 3:
				result = startGrandchild(xmlStack, thisTag, atts);
				break;
	
			default:
				result = startFurtherChild(xmlStack, thisTag, lastTag, atts);
			}
		} else {
			System.out.println("Defaulting to ignore for other namespace " + thisTag.uri);
			XMPPReaderAdapter newHandler = new XMPPReaderIgnore(this, null);
			connection.setXMPPReader(newHandler);
			result = newHandler.startElement(xmlStack, thisTag, null, atts);
		}
		
		if (oldStackSize != xmlStack.size()) {
			throw new Error();
		}
		return result;
	}
		
	protected abstract boolean startTopLevel(Stack xmlStack, AbstractXMLTag thisTag, Attributes atts) throws SAXException;
	protected abstract boolean startChild(Stack xmlStack, AbstractXMLTag thisTag, Attributes atts)  throws SAXException;
	protected abstract boolean startGrandchild(Stack xmlStack, AbstractXMLTag thisTag, Attributes atts) throws SAXException;
	protected abstract boolean startFurtherChild(Stack xmlStack, AbstractXMLTag thisTag, AbstractXMLTag lastTag, Attributes atts) throws SAXException;


	
	public final void endElement(String uri, String localName, String name)
			throws SAXException {
		
		System.out.println("endElement: " + localName + "(" + uri + ")");
		AbstractXMLTag thisTag = xmlStack.pop();
		if (!(uri.equals(thisTag.uri) && localName.equals(thisTag.getName()))) {
			// XML error, a tag is closed that was not opened
			xmlStack.add(thisTag);
			throw new InvalidXMLException(xmlStack, new XMLTag(localName, uri));
		}
		AbstractXMLTag lastTag = xmlStack.peek();
		
		String content = contentCharacters != null ? contentCharacters.toString() : null;
		contentCharacters = null;

		try {
			boolean result;
			int oldStackSize = xmlStack.size();
			switch (oldStackSize-offset) {
			case 0: 
				result = endTopLevel(thisTag, content);
				break;
				
			case 1:
				result = endChild(xmlStack, thisTag, content);
				break;
				
			case 2:
				result = endGrandchild(xmlStack, thisTag, content);
				break;

			default:
				result = endFurtherChild(xmlStack, thisTag, lastTag, content);
			}
			
			if (oldStackSize != xmlStack.size()) {
				throw new Error();
			}
			
			if (!result) {
				// TODO: what to do here?
				// assert false : "Only happens if we forgot to handle a tag here that was handled in startElement()";
				xmlStack.add(thisTag);
				throw new UnknownXMLException(xmlStack);
			}

		} catch (IOException e) {
			throw new SAXException(e);
		}
	}

	protected abstract boolean endTopLevel(AbstractXMLTag thisTag, String content) throws IOException, SAXException;
	protected abstract boolean endChild(Stack xmlStack, AbstractXMLTag thisTag, String content) throws IOException, SAXException;
	protected abstract boolean endGrandchild(Stack xmlStack, AbstractXMLTag thisTag, String content) throws IOException, SAXException;
	protected abstract boolean endFurtherChild(Stack xmlStack, AbstractXMLTag thisTag, AbstractXMLTag lastTag, String content) throws IOException, SAXException;


	
	public void endPrefixMapping(String prefix) throws SAXException {
		/* ignore */
	}

	
	public void ignorableWhitespace(char[] ch, int start, int length)
			throws SAXException {
		/* ignore */
	}

	
	public void processingInstruction(String target, String data)
			throws SAXException {
		/* ignore */
	}

	
	public void setDocumentLocator(Locator locator) {
		/* ignore */
	}

	
	public void skippedEntity(String name) throws SAXException {
		/* ignore */
	}

	
	public void startPrefixMapping(String prefix, String uri)
			throws SAXException {
		/* ignore */
	}
}
