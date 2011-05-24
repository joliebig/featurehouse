


import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.IOException;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JTextArea;
import javax.swing.JTextField;

public class ClientGUI extends JFrame {
	/**
	 * 
	 */
	private static final long serialVersionUID = 2565422293410796014L;

	private JTextArea messages;
	private JTextField sendMessage;
	private JButton sendButton;
	
	protected Client client;
	
	protected final String clientName = "b" + Math.random();
	
	public ClientGUI() throws Exception {
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		messages = new JTextArea();
		sendMessage = new JTextField();
		sendButton = new JButton("send");
		
		client = new Client();
		
		setLayout(new GridLayout(6,1));
		
		messages.setEditable(false);
		sendButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String msg = sendMessage.getText();
				msg = preMessageSent(msg);
				
				synchronized(client) {
					try {
						client.sendMessage(msg);
						sendMessage.setText("");
					} catch (IOException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
				}
			}
		});
		
		add(messages);
		add(sendMessage);
		add(sendButton);
		
		initGUI();	
		
		
		pack();
		setVisible(true);
		setSize(300, 700);
		
		
		new Thread() {
			public void run() {
				while (true) {
					synchronized(client) {
						try {
							String newMessage = client.getMessage();
							if (newMessage == null) 
								newMessage = "";
							
							newMessage = postMessageReceived(newMessage);
							messages.setText(newMessage + messages.getText());
						} catch (IOException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
					try {
						Thread.sleep(1000);
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
						return;
					}
				}
			}
		}.start();
	}
	
	public String preMessageSent(String msg) {
		return msg;
	}
	
	public String postMessageReceived(String newMessage) {
		return newMessage;
	}
	
	public void initGUI() throws Exception {
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			new ClientGUI();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
