package de.uni_passau.fim.pkjab.view;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.text.DateFormat;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.ImageIcon;

import de.uni_passau.fim.pkjab.model.Contact;
import de.uni_passau.fim.pkjab.model.messages.Message;
import de.uni_passau.fim.pkjab.util.listener.ContactListener;
import de.uni_passau.fim.pkjab.util.listener.ContactStateEvent;
import de.uni_passau.fim.pkjab.util.listener.MessageReceivedEvent;


public class MessageFrame extends JFrame implements ContactListener {
	
	protected static final String SUBMIT = "Senden";
	
	protected static final String STATUS_IS = "Status: ";
	
	protected final Contact contact;
	
	protected final String userNick;
	
	protected JTextArea composingArea;
	
	private JTextArea historyArea;
	
	protected final JPanel bottomPanel = new JPanel();
	
	MessageFrame(final Contact contact, final String userNick) {
		
		super(contact.getNick() + " (" + STATUS_IS + contact.getState() + ")");
		
		this.contact = contact;
		this.userNick = userNick;
		
		contact.addContactListener(this);
		
		setupGui();
		setupListeners();
		
		setPreferredSize(new Dimension(500, 400));
		setIconImage(((ImageIcon) PKjabToolkit.getStatusIcons().get(contact.getState())).getImage());
		final Dimension dim = Toolkit.getDefaultToolkit().getScreenSize();
		setLocation((dim.width - 500) / 2, 
				(dim.height - 400) / 2);
		
		setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
		
		pack();
	}
	
	private void setupGui() {

		setLayout(new BorderLayout());
		
		
		final JButton submitButton = new JButton(SUBMIT);
		submitButton.addActionListener(new ActionListener() {

			public void actionPerformed(final ActionEvent e) {
			
				if (!composingArea.getText().equals("")) {
					addToHistory(messageToText(false, contact.sendMessage(composingArea.getText())));
					
					composingArea.setText("");
				}
			}

		});
		
		
		bottomPanel.setLayout(new BorderLayout(10, 1));
		bottomPanel.add(submitButton, BorderLayout.LINE_END);

		
		composingArea = new JTextArea();
		composingArea.setCaretColor(PKjabToolkit.getTheme().getForegroundColor());
		composingArea.addKeyListener(new KeyAdapter() {
			
			public void keyPressed(final KeyEvent e) {

				if (e.isControlDown() && e.getKeyCode() == KeyEvent.VK_ENTER)
					submitButton.doClick();
			}
		});
		
		historyArea = new JTextArea();
		historyArea.setEditable(false);
		historyArea.setText(DateFormat
				.getDateTimeInstance(DateFormat.LONG, DateFormat.LONG)
				.format(new java.util.Date()));
		
		final JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
				new JScrollPane(historyArea), new JScrollPane(composingArea));
		splitPane.setOneTouchExpandable(true);
		splitPane.setResizeWeight(0.8);

		getContentPane().add(splitPane, BorderLayout.CENTER);
		getContentPane().add(bottomPanel, BorderLayout.PAGE_END);
	}
	
	private void setupListeners() {
		
		addComponentListener(new ComponentAdapter() {
			
			public void componentShown(final ComponentEvent e) {
				composingArea.requestFocusInWindow();
			}
		});
	}

	public void contactStateChanged(final ContactStateEvent e) {
		setIconImage( ((ImageIcon) PKjabToolkit.getStatusIcons().get(e.getState())).getImage());
		setTitle(contact.getNick() + " (" + STATUS_IS + e.getState() + ")");
	}

	public void messageReceived(final MessageReceivedEvent e) {
		addToHistory(messageToText(true, e.getMessage()));
	}
	
	protected void addToHistory(String text) {
		historyArea.append(System.getProperty("line.separator") + text);
		historyArea.setCaretPosition(historyArea.getText().length());
	}
	
	protected String messageToText(boolean incoming, Message message) {
		StringBuffer sb = new StringBuffer("<");
		
		if (incoming)
			sb.append(contact.getNick() + "/" + message.getFrom().getResource());
		else
			sb.append(userNick);
		
		sb.append("> ");
		sb.append(message.toString());
		return sb.toString();
	}

}
