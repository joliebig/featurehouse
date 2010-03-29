
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.Timer;

import pkjab.de.uni_passau.fim.pkjab.model.Contact;
import pkjab.de.uni_passau.fim.pkjab.util.Jid;
import pkjab.de.uni_passau.fim.pkjab.util.listener.ContactListener;
import pkjab.de.uni_passau.fim.pkjab.util.listener.ContactStateEvent;
import pkjab.de.uni_passau.fim.pkjab.util.listener.MessageReceivedEvent;

public class ContactPanel extends JButton implements ContactListener {

	private final Contact contact;
	
	private MessageFrame messageFrame;
	
	private static final String STATUS_IS = "Status: ";
		
	private static final String ICON_MESSAGE = PKjabToolkit.ICON_DIR + "message.png";
	
	private static final String ICON_BLANK = PKjabToolkit.ICON_DIR + "blank.png";
	
	protected ImageIcon typingIcon;
	
	protected ImageIcon messageIcon;
	
	protected ImageIcon statusIcon;
	
	protected ImageIcon blankIcon;
	
	private static final int timerDelay = 1000;
	
	private boolean isStatusShowing = true;
	
	private JLabel statusLabel;
	
	private JLabel nickLabel;
	
	private Timer blinkTimer;
	
	public ContactPanel(final Contact contact, final String userNick) {
		super();
		
		this.contact = contact;
		
		contact.addContactListener(this);
		
		readImageIcons();
		createLabels();
		createLayout();
		initTimer();
		defineAction();
		
		messageFrame = new MessageFrame(contact, userNick);
	}
	
	private void readImageIcons() {
		statusIcon = (ImageIcon) PKjabToolkit.getStatusIcons().get(contact.getState());
		messageIcon = PKjabToolkit.getImageIcon(ICON_MESSAGE);
		blankIcon = PKjabToolkit.getImageIcon(ICON_BLANK);
	}
	
	private void createLabels() {
		
		statusLabel = new JLabel(statusIcon);
		
		nickLabel = new JLabel(contact.getNick());
	}
	
	private void createLayout() {
		
		setAlignmentX(Component.LEFT_ALIGNMENT);
		
		setLayout(new BorderLayout(5, 1));
		
		add(statusLabel, BorderLayout.LINE_START);
		add(nickLabel, BorderLayout.CENTER);
		
		String statusText = contact.getStateText();
		if (statusText == null || statusText == "") 
			statusText = contact.getState().toString();
		setToolTipText(STATUS_IS + statusText);
	}
	
	private void initTimer() {
		blinkTimer = new Timer(timerDelay,
				new ActionListener() {

					public void actionPerformed(ActionEvent e) {
						if (isStatusShowing)
							statusLabel.setIcon(messageIcon);
						else
							statusLabel.setIcon(statusIcon);
						
						isStatusShowing = !isStatusShowing;
					}
			
		});
		
		blinkTimer.setInitialDelay(0);
	}
	
	private void defineAction() {
		
		addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				
				if (blinkTimer.isRunning())
					blinkTimer.stop();
				
				statusLabel.setIcon(statusIcon);
				
				if (!messageFrame.isShowing())
					messageFrame.setVisible(true);
			}
			
		});
	}

	public void contactStateChanged(ContactStateEvent e) {
		statusIcon = (ImageIcon) PKjabToolkit.getStatusIcons().get(e.getState());
		statusLabel.setIcon(statusIcon);
		setToolTipText(STATUS_IS + contact.getStateText());
		nickLabel.setText(contact.getNick());
	}

	public void messageReceived(MessageReceivedEvent e) {
		if (!messageFrame.isShowing())
			blinkTimer.start();
	}
}
