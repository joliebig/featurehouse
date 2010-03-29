
import java.util.EventObject;

import pkjab.de.uni_passau.fim.pkjab.model.Contact;
import pkjab.de.uni_passau.fim.pkjab.util.ChatState;

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
