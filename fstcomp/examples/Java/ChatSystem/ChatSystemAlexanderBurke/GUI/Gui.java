

import java.awt.Button;
import java.awt.Color;
import java.awt.Event;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.TextField;

import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

public class Gui extends Frame {

	protected JTextPane outputTextbox;
	protected TextField inputField;
	private Client client = null;
	
	protected StyledDocument doc;
	protected Style style;
	
	public Gui(Client client) {
		
		System.out.println("starting gui...");
		this.client = client;
		createLayout();
	}
	
	public void createLayout() {
		
		setLayout(new GridBagLayout());
			
		Insets insets = new Insets(0, 0, 0, 0);
			
		outputTextbox = new JTextPane();		
		outputTextbox.setEditable(false);
		doc = outputTextbox.getStyledDocument();
		style = doc.addStyle("rgb", null);
		StyleConstants.setForeground(style, Color.black);
		
		add(new JScrollPane(outputTextbox), new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0, 
				GridBagConstraints.NORTHWEST, GridBagConstraints.BOTH, insets, 0, 0));
		
		inputField = new TextField();
		add(inputField, new GridBagConstraints(0, 1, 1, 1, 1.0, 0, GridBagConstraints.SOUTH, GridBagConstraints.HORIZONTAL, insets, 0, 0));
		
		setSize(400, 300);
		setVisible(true);
		inputField.requestFocus();
	}
	
	/**
	 * handles AWT events (enter in textfield and closing window)
	 */
	public boolean handleEvent(Event e) {
		if ((e.target == inputField) && (e.id == Event.ACTION_EVENT)) {
			String msg = (String) e.arg;
			
			send(msg);
			inputField.setText("");
			return true;
		} else if ((e.target == this) && (e.id == Event.WINDOW_DESTROY)) {
			
			if (client != null)
				client.stop();
			
			setVisible(false);
			System.exit(0);
			return true;
		}
		return super.handleEvent(e);
	}
	
	public void send(String msg) {
		client.send(msg);
	}
	
	public void onMessageReceived(String text) {

		try {
			// Text einf�gen
			doc.insertString(doc.getLength(), text, style);
			// Zeilenumbruch einf�gen
			doc.insertString(doc.getLength(), "\n", style);
			// Und bis ans Ende scrollen
			outputTextbox.setCaretPosition(doc.getLength());	
		} catch (BadLocationException e) {
			e.printStackTrace();
			outputTextbox.setText(outputTextbox.getText() + text + "\n");
		}

		
	}
}