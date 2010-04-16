


import java.awt.Button;
import java.awt.Color;
import java.awt.Component;
import java.awt.Event;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.TextField;

import javax.swing.DefaultListModel;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.ListCellRenderer;

/**
 * simple AWT gui for the chat client
 */
public class Gui extends Frame implements ChatLineListener {

	private static final long serialVersionUID = 1L;

	protected DefaultListModel outputList;

	protected TextField inputField;
	
	private Client chatClient;
	

	//<--CODING MUNGE HAT KEIN "OR" DESWEGEN BLEIBT ES DRIN!
	//protected JComboBox coding1;
	//protected JComboBox coding2;
	//<--CODING
	
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
		System.out.println("starting gui...");
		
		setLayout(new GridBagLayout());
		outputList = new DefaultListModel();
		JList list = new JList(outputList);
		
		list.setCellRenderer(new TextMessageRenderer());
		
		add(new JScrollPane(list), new GridBagConstraints(0,0,1,4,1.0,1.0,GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0,0,0,0), 0, 0));
		
		inputField = new TextField();
		add(inputField, new GridBagConstraints(0,4,2,1,1.0,0,GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0,0,0,0),0,0));

		// register listener so that we are informed whenever a new chat message
		// is received (observer pattern)
		chatClient.addLineListener(this);

		this.chatClient = chatClient;
		
		addGraphicalElements();
		
		
		
		//-->CODING MUNGE HAT KEIN "OR" DESWEGEN BLEIBT ES DRIN!
		//coding1 = new JComboBox(Utils.getCodingList());
		//add(coding1, new GridBagConstraints(1,1,1,1,0,0,GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0,0,0,0),0,0));
		//coding2 = new JComboBox(Utils.getCodingList());	
		//add(coding2, new GridBagConstraints(1,2,1,1,0,0,GridBagConstraints.WEST, GridBagConstraints.BOTH, new Insets(0,0,0,0),0,0));
		//<--CODING
		
		
		pack();
		setVisible(true);
		inputField.requestFocus();
		
	}

	protected void addGraphicalElements() {
	}

	/**
	 * this method gets called every time a new message is received (observer
	 * pattern)
	 */
	public void newChatLine(TextMessage msg) {
		outputList.addElement(msg);
	}

	/**
	 * handles AWT events (enter in textfield and closing window)
	 */
	public boolean handleEvent(Event e) {
		if ((e.target == inputField) && (e.id == Event.ACTION_EVENT)) {
			
			TextMessage txtMsg = new TextMessage((String) e.arg);
			
			modifyTextMessage(txtMsg);
			
			chatClient.send(txtMsg);
			inputField.setText("");
			return true;
		} 
		
		else if ((e.target == this) && (e.id == Event.WINDOW_DESTROY)) {
			if (chatClient != null)
				chatClient.stop();
			setVisible(false);
			System.exit(0);
			return true;
		}
		return super.handleEvent(e);
	}
	
	protected void modifyTextMessage(TextMessage txtMsg) {
			
			//<--CODING MUNGE HAT KEIN "OR" DESWEGEN BLEIBT ES DRIN!
			/*
			String tmpEntry;
			
			if (!(tmpEntry = coding1.getSelectedItem().toString()).equals(""))
				txtMsg.addSetting(Utils.CODING1, tmpEntry);
			
			if (!(tmpEntry = coding2.getSelectedItem().toString()).equals(""))
				txtMsg.addSetting(Utils.CODING2, tmpEntry);
			*/
			//<--CODING
	}
	
}
