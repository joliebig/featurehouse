
class XMPPIqHandler {

	private static final XMLTag ROSTER_QUERY_TAG = new XMLTag("query", ROSTER_URI);
	
	protected boolean startChild(Stack xmlStack, AbstractXMLTag thisTag,
			Attributes atts) throws SAXException {
		boolean result = super.startChild(xmlStack, thisTag, atts);
		Iq iq = (Iq) xmlStack.get(1);
		if (thisTag.equals(ROSTER_QUERY_TAG)) {
			XMPPReaderAdapter newHandler = new XMPPRosterHandler(this, xmlStack);
			connection.setXMPPReader(newHandler);
			return newHandler.startElement(xmlStack, thisTag, null, atts);
		}
		return result;
	}
}