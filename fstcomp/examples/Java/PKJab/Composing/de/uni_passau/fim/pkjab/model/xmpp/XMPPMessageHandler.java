

import pkjab.de.uni_passau.fim.pkjab.util.ChatState;

class XMPPMessageHandler {

	private static final String CHATSTATES_URI = "http://jabber.org/protocol/chatstates";

 protected boolean startChild(Stack xmlStack,
			AbstractXMLTag thisTag, Attributes atts) throws SAXException {
		if (thisTag.uri.equals(CHATSTATES_URI)) {
			contact.setChatState(ChatState.valueOf(thisTag.getName().trim().toUpperCase()));
			return true;
		} else {
			return original(xmlStack, thisTag, atts);
		}
	}

 protected boolean endChild(Stack xmlStack,
			AbstractXMLTag thisTag, String content) {
		if (thisTag.uri.equals(CHATSTATES_URI)) {
			return true;
		} else {
			return original(xmlStack, thisTag, content);
		}
	}
}