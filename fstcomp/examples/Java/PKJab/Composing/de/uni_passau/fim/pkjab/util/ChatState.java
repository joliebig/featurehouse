package de.uni_passau.fim.pkjab.util;

public final class ChatState {

	public static final ChatState ACTIVE = new ChatState();
	public static final ChatState COMPOSING = new ChatState();
	public static final ChatState INACTIVE = new ChatState(); 
	public static final ChatState PAUSED = new ChatState();
	public static final ChatState GONE = new ChatState();
	
	protected ChatState() { }
	
	public static ChatState valueOf(String chatState) {
		if (chatState.equals("ACTIVE")) {
			return ACTIVE;
		} else if (chatState.equals("COMPOSING")) {
			return COMPOSING;
		} else if (chatState.equals("INACTIVE")) {
			return INACTIVE;
		} else if (chatState.equals("PAUSED")) {
			return PAUSED;
		} else if (chatState.equals("GONE")) {
			return GONE;
		} else {
			throw new IllegalArgumentException();
		}
	}
}
