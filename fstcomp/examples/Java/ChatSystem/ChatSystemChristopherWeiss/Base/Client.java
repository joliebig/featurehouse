

import java.awt.Color;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Iterator;



/**
 * simple chat client
 */
public class Client implements Runnable {

	protected ObjectInputStream inputStream;

	protected ObjectOutputStream outputStream;

	protected Thread thread;
	
	protected StartingGui gui;
	
	protected String name;

	public Client(String host, int port,String name, StartingGui gui) {
		
		this.gui = gui;
		this.name = name;
		
		try {
			System.out.println("Connecting to " + host + " (port " + port
					+ ")...");
			Socket s = new Socket(host, port);
			this.outputStream = new ObjectOutputStream((s.getOutputStream()));
			this.inputStream = new ObjectInputStream((s.getInputStream()));
			thread = new Thread(this);
			thread.start();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * main method. waits for incoming messages.
	 */
	public void run() {
		try {
			while (true) {
				try {

					Object msg = inputStream.readObject();
					handleIncomingMessage(msg);
				} catch (ClassNotFoundException e) {
					e.printStackTrace();
				}

			}
		} catch (IOException ex) {
			ex.printStackTrace();
		} finally {
			thread = null;
			try {
				outputStream.close();
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}
	}

	/**
	 * decides what to do with incoming messages
	 * 
	 * @param msg
	 *            the message (Object) received from the sockets
	 */
	protected void handleIncomingMessage(Object msg) {
		if (msg instanceof AuthMessage)
		{
			gui.checkPassword(((AuthMessage) msg).getContent());
		}
		if (msg instanceof TextMessage) 
		{
			fireAddLine(((TextMessage) msg).getSender() + ": ", ((TextMessage) msg).getContent()
						+ "\n", ((TextMessage) msg).getColor());
			
		}
	}

	public void send(String from, String line, Color color) {
		send(new TextMessage(from, line, color));
	}

	public void send(Message msg) {
		try {
			outputStream.writeObject(msg);
			outputStream.flush();
		} catch (IOException ex) {
			ex.printStackTrace();
			thread.stop();
		}
	}
	
	/**
	 * listener-list for the observer pattern
	 */
	private ArrayList listeners = new ArrayList();

	/**
	 * addListner method for the observer pattern
	 */
	public void addListener(ChatListener listener) {
		listeners.add(listener);
	}

	/**
	 * removeListner method for the observer pattern
	 */
	public void removeListener(ChatListener listener) {
		listeners.remove(listener);
	}

	/**
	 * fire Listener method for the observer pattern
	 */

	public void fireAddLine(String sender, String line, Color color) {
		for (Iterator iterator = listeners.iterator(); iterator.hasNext();) {
			ChatListener listener = (ChatListener) iterator.next();
			listener.newChatLine(sender, line, color);
		}
	}

	public void stop() {
		thread.stop();
	}

}