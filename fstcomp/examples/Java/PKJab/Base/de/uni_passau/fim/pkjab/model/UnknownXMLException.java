
import org.xml.sax.SAXException;

import pkjab.de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import pkjab.de.uni_passau.fim.pkjab.util.Stack;

public class UnknownXMLException extends SAXException {

	private static final long serialVersionUID = 7855598945244855783L;

	private final Stack xmlStack; // <AbstractXMLTag>

	public UnknownXMLException(final Stack xmlStack) {
		this.xmlStack = xmlStack;
	}

	public String getMessage() {
		return xmlStack.toString();
	}
}