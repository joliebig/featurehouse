package de.uni_passau.fim.pkjab.util.listener;

import java.util.EventObject;

import de.uni_passau.fim.pkjab.model.Contact;
import de.uni_passau.fim.pkjab.util.ChatState;

public class ContactComposingEvent extends EventObject {
	
	private ChatState state;
	
	public ContactComposingEvent(Contact source, ChatState chatState) {
		super(source);
		
		state = chatState;
	}

	public ChatState getState() {
		return state;
	}
	
	public Contact getSource() {
		return (Contact) super.getSource();
	}
}
