

import java.awt.BorderLayout;
import java.awt.Event;
import java.awt.Frame;
import java.awt.TextArea;
import java.awt.TextField;
import java.util.Random;


/**
 * simple AWT gui for the chat client
 */
public class Gui {

	protected TextField colorField;

	protected void init(Client chatClient) {
		original(chatClient);
		colorField = new TextField();
		colorField.setText(new String[] {"rot", "blau", "gelb"}[new Random().nextInt(3)]);
		add("Center", colorField);
	}

	/**
	 * handles AWT events (enter in textfield and closing window)
	 */
	public boolean handleEvent(Event e) {
		if ((e.target == inputField) && (e.id == Event.ACTION_EVENT)) {
			String text = (String) e.arg;
			text = "[" + colorField.getText() + "] " + text;
			chatClient.send(new TextMessage(text));
			inputField.setText("");
			return true;
		}
		return original(e);
	}
}
