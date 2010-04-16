

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.awt.event.WindowAdapter;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.Color;
import java.awt.Event;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;

import java.awt.TextArea;
import java.awt.TextField;

import javax.swing.JButton;

import javax.swing.JFrame;
import javax.swing.JLabel;

import javax.swing.JComponent;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.JScrollPane;

import javax.swing.*;
import javax.swing.text.*;



/**
 * simple AWT gui for the chat client
 */
public class ChatGui extends JFrame implements ChatListener, ActionListener,
		WindowListener {

	private static final long serialVersionUID = 1L;

	protected TextArea outputTextbox;

	protected JTextField inputField;

	protected JTextField passwordField;

	protected JLabel passwordLabel;

	protected JButton sendButton;

	protected JTextPane textPane;

	protected JFrame passwordFrame;

	private Client chatClient;

	private StyledDocument doc;
	private Style style;

	protected Color currentColor;
	
	private String username;
	
	protected JPanel panel;
	
	protected JScrollPane scrollPane;
	
	/**
	 * creates layout
	 * 
	 * @param title
	 *            title of the window
	 * @param chatClient
	 *            chatClient that is used for sending and receiving messages
	 */
	public ChatGui(String title, Client chatClient,String name) {
		super(title);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		currentColor = Color.black;
		username = name;
		
		System.out.println("starting gui...");
		
		setLayout(new BorderLayout());

		addCom();
	
		add("East",panel);
		add("Center", scrollPane);
	    add("South", inputField);
		
		// register listener so that we are informed whenever a new chat message
		// is received (observer pattern)
		chatClient.addListener(this);

		pack();
		this.setSize(400, 400);
		setVisible(true);
		inputField.requestFocus();

		this.chatClient = chatClient;
	}

	void addCom()
	{
		textPane = new JTextPane();
		textPane.setEditable(false);
		scrollPane = new JScrollPane(textPane,
				JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

		doc = (StyledDocument) textPane.getDocument();

		// Create a style object and then set the style attributes
		style = doc.addStyle("StyleName", null);
		
		inputField = new JTextField();
		inputField.addActionListener(this);
		
		panel = new JPanel();
		panel.setLayout(new FlowLayout());
	}



	/**
	 * this method gets called every time a new message is received (observer
	 * pattern)
	 */
	public void newChatLine(String sender,String line, Color color) {

		// Append to document
		String text = sender.concat(line);
		StyleConstants.setForeground(style, color);
		try {
			doc.insertString(doc.getLength(), text, style);
		} catch (Exception e) {

		}
		
	}

	public void windowClosing(WindowEvent e) {
		chatClient.stop();
		setVisible(false);
		dispose();
		System.exit(0);
	}

	public void windowActivated(WindowEvent e) {
	}

	public void windowClosed(WindowEvent e) {
	}

	public void windowDeactivated(WindowEvent e) {
	}

	public void windowDeiconified(WindowEvent e) {
	}

	public void windowIconified(WindowEvent e) {
	}

	public void windowOpened(WindowEvent e) {
	}

	//@Override
	public void actionPerformed(ActionEvent e) {
		if ((e.getSource() == inputField)) {
			chatClient.send(username, inputField.getText(), currentColor);
			inputField.setText("");
			
		} 

	}

	

}
