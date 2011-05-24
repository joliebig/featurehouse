package de.uni_passau.fim.pkjab.model;

import de.uni_passau.fim.pkjab.util.ChatState;
import de.uni_passau.fim.pkjab.util.listener.ContactComposingEvent;

class Contact {

	private ChatState chatState = null;

	private synchronized void fireContactComposing(ContactComposingEvent e) {
		EventListener[] listener = listeners.getListeners(ContactListener.class);
		for (int i = 0; i < listener.length; i++ )
			((ContactListener) listener[i]).contactTyping(e);
	}
	
	public void setChatState(ChatState chatState) {
		fireContactComposing(new ContactComposingEvent(this, chatState));
	}
	
 public void fireStateChanged() {
		original();
		if ((chatState != null) && (getState() == UserState.OFFLINE) && (chatState != ChatState.GONE)) {
			setChatState(ChatState.GONE);
		}
	}

	public ChatState getChatState() {
		return chatState;
	}
}