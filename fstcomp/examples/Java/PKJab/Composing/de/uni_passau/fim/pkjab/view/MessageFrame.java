package de.uni_passau.fim.pkjab.view;

import de.uni_passau.fim.pkjab.util.ChatState;
import de.uni_passau.fim.pkjab.util.listener.ContactComposingEvent;

class MessageFrame {

	protected final JLabel notificationLabel = new JLabel();

	private static final String IS_COMPOSING = " schreibt gerade";
	
	public void contactTyping(final ContactComposingEvent e) {
		
		if (e.getState() == ChatState.COMPOSING) {
			notificationLabel.setText(contact.getNick() + IS_COMPOSING);
		}
		else {
			notificationLabel.setText("");
		}		
	}
	
	MessageFrame(final Contact contact, final String userNick) {
		notificationLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
		bottomPanel.add(notificationLabel, BorderLayout.LINE_START);
	}
}
