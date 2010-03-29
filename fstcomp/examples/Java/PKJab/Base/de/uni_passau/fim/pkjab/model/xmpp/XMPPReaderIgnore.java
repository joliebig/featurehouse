
import java.io.IOException;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import pkjab.de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import pkjab.de.uni_passau.fim.pkjab.util.Stack;

class XMPPReaderIgnore extends XMPPReaderSubLevel {

	protected XMPPReaderIgnore(final XMPPReaderAdapter previousHandler,
			final String namespace) {
		super(previousHandler, namespace);
	}
	

	protected boolean startChild(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		super.startChild(xmlStack, thisTag, atts);
		return true;
	}


	protected boolean startGrandchild(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		super.startGrandchild(xmlStack, thisTag, atts);
		return true;
	}


	protected boolean startFurtherChild(Stack xmlStack,
			AbstractXMLTag thisTag, AbstractXMLTag lastTag, Attributes atts)
			throws SAXException {
		super.startFurtherChild(xmlStack, thisTag, lastTag, atts);
		return true;
	}

	

	protected boolean endChild(Stack xmlStack,
			AbstractXMLTag thisTag, String content) throws IOException,
			SAXException {
		super.endChild(xmlStack, thisTag, content);
		return true;
	}


	protected boolean endGrandchild(Stack xmlStack,
			AbstractXMLTag thisTag, String content)
			throws IOException, SAXException {
		super.endGrandchild(xmlStack, thisTag, content);
		return true;
	}


	protected boolean endFurtherChild(Stack xmlStack,
			AbstractXMLTag thisTag, AbstractXMLTag lastTag, String content)
			throws IOException, SAXException {
		super.endFurtherChild(xmlStack, thisTag, lastTag, content);
		return true;
	}	
}
