
import pkjab.de.uni_passau.fim.pkjab.util.ChatState;
import pkjab.de.uni_passau.fim.pkjab.util.listener.ContactComposingEvent;

class ContactPanel {

	private static final String ICON_TYPING = PKjabToolkit.ICON_DIR + "typing.png";

	private JLabel typingLabel;
	
	public void contactTyping(ContactComposingEvent e) {
		
		if (e.getState() == ChatState.COMPOSING)
			typingLabel.setIcon(typingIcon);
		else
			typingLabel.setIcon(blankIcon);
	}
	
	ContactPanel(final Contact contact, final String userNick) {
		typingIcon = PKjabToolkit.getImageIcon(ICON_TYPING);
		typingLabel = new JLabel(blankIcon);
		add(typingLabel, BorderLayout.LINE_END);	
	}
}
