

import java.awt.BorderLayout;
import java.awt.Event;
import java.awt.Frame;
import java.awt.TextArea;
import java.awt.TextField;
import java.util.Random;


/**
 * simple AWT gui for the chat client
 */
public class Gui extends Frame implements ChatLineListener {

	private static final long serialVersionUID = 1L;

	protected TextArea outputTextbox;

	protected TextField inputField;

	protected Client chatClient;

	/**
	 * creates layout
	 * 
	 * @param title
	 *            title of the window
	 * @param chatClient
	 *            chatClient that is used for sending and receiving messages
	 */
	public Gui(String title, Client chatClient) {
		super(title);
		this.chatClient = chatClient;
		init(chatClient);
		start();
	}

	protected void init(Client chatClient) {
		// register listener so that we are informed whenever a new chat message
		// is received (observer pattern)
		outputTextbox = new TextArea();
		chatClient.addLineListener(this);

		System.out.println("starting gui...");
		setLayout(new BorderLayout());
		add("North", outputTextbox);
		outputTextbox.setEditable(false);
		inputField = new TextField();
		add("South", inputField);
	}
	
	protected void start() {
		pack();
		setVisible(true);
		inputField.requestFocus();
	}

	/**
	 * this method gets called every time a new message is received (observer
	 * pattern)
	 */
	public void newChatLine(String line) {
		outputTextbox.append(line);
	}

	/**
	 * handles AWT events (enter in textfield and closing window)
	 */
	public boolean handleEvent(Event e) {
		if ((e.target == inputField) && (e.id == Event.ACTION_EVENT)) {
			String text = (String) e.arg;
			chatClient.send(new TextMessage(text));
			inputField.setText("");
			return true;
		} else if ((e.target == this) && (e.id == Event.WINDOW_DESTROY)) {
			if (chatClient != null)
				chatClient.stop();
			setVisible(false);
			System.exit(0);
			return true;
		}
		return super.handleEvent(e);
	}
}
