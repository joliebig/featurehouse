

import pkjab.de.uni_passau.fim.pkjab.util.ChatState;

public class ChatStateMessage extends Message {

	private ChatState chatState = ChatState.ACTIVE;
	private String type = null;
	
	public void setChatState(ChatState state) {
		if (state == null) {
			throw new IllegalArgumentException();
		}
		chatState = state;
	}
	
	public ChatState getChatState() {
		return chatState;
	}

	public String toXML() {
		return String.format("<message from='%s' to='%s' type='chat'><%s xmlns='http://jabber.org/protocol/chatstates'/></message>", getFrom(), getTo(), chatState.toString().toLowerCase());
	}
}