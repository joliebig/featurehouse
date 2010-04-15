

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
	public static void main(String args[]) throws IOException {
		if (args.length != 2)
			throw new RuntimeException("Syntax: ChatClient <host> <port>");

		Client client = new Client(args[0], Integer.parseInt(args[1]));
		
		new Gui("Chat " + args[0] + ":" + args[1], client);
	}

	protected ObjectInputStream inputStream;

	protected ObjectOutputStream outputStream;

	protected Thread thread;
	
	protected boolean error = false;
	protected boolean stop = false;

	public Client(String host, int port) {
		try {			
			System.out.println("Connecting to " + host + " (port " + port
					+ ")...");
			Socket s = new Socket(host, port);
			this.outputStream = new ObjectOutputStream((s.getOutputStream()));
			this.inputStream = new ObjectInputStream((s.getInputStream()));
			thread = new Thread(this);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public void start() {	
			thread.start();
	}

	/**
	 * main method. waits for incoming messages.
	 */
	public void run() {
		try {
			while (!(error || stop)) {
				try {
					Object msg = inputStream.readObject();
					System.out.println("RECEIVED");
					handleIncomingMessage(msg);
				} catch (ClassNotFoundException e) {
					// TODO Auto-generated catch block
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

	protected String getMessageContent(TextMessage msg) {
		TextMessage message = (TextMessage)msg;
		String text = "";

		text += message.getSender() + " wrote : ";
		text += message.getContent();
		
		return text;
	}
	
	protected void propagateMessage(TextMessage msg) {
		fireAddLine(getMessageContent(msg) + "\n");
	}
	
	/**
	 * decides what to do with incoming messages
	 * 
	 * @param msg
	 *            the message (Object) received from the sockets
	 */
	protected void handleIncomingMessage(Object msg) {
		System.out.println(msg + ":" + (msg instanceof TextMessage));
		if (msg instanceof TextMessage) {
			propagateMessage((TextMessage)msg);
		}
	}
	
	public void send(String line) {
		TextMessage message = new TextMessage(line);
			
		send(message);
	}

	public void send(ChatMessage msg) {
		try {
			outputStream.writeObject(msg);
			outputStream.flush();
		} catch (IOException ex) {
			System.out.println(ex.getMessage());
			ex.printStackTrace();
			error = true;
		}
	}

	/**
	 * listener-list for the observer pattern
	 */
	protected ArrayList listeners = new ArrayList();

	/**
	 * addListner method for the observer pattern
	 */
	public void addLineListener(ChatLineListener listener) {
		listeners.add(listener);
	}

	/**
	 * removeListner method for the observer pattern
	 */
	public void removeLineListener(ChatLineListener listner) {
		listeners.remove(listner);
	}

	/**
	 * fire Listner method for the observer pattern
	 */
	public void fireAddLine(String line) {
		System.out.println("FIRE: " + listeners.size());
		for (Iterator iterator = listeners.iterator(); iterator.hasNext();) {
			ChatLineListener listener = (ChatLineListener) iterator.next();
			listener.newChatLine(line);
		}
	}

	public void stop() {
		stop = true;
	}
}
