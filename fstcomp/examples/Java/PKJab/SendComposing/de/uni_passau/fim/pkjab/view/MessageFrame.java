
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

class MessageFrame implements DocumentListener {

	MessageFrame(final Contact contact, final String userNick) {
		composingArea.getDocument().addDocumentListener(this);
	}

	public void changedUpdate(DocumentEvent e) { }
	
	public void insertUpdate(DocumentEvent e) {
		if (e.getLength() > 0) {
			contact.setMyChatState(ChatState.COMPOSING);
		}
	}
	
	public void removeUpdate(DocumentEvent e) {
		if (e.getDocument().getLength() == 0) {
			contact.setMyChatState(ChatState.ACTIVE);
		}
	}
}
