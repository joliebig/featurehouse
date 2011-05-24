package de.uni_passau.fim.pkjab.model.messages;

class TextMessage {

	private static final String ACTIVE_STATE = "<active xmlns='http://jabber.org/protocol/chatstates'/>";

 public String toXML() {
		String result = original();
		String[] parts = result.split(">", 2);
		return parts[0] + ">" + ACTIVE_STATE + parts[1];
	}
	
}