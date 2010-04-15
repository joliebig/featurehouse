

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * simple chat client
 */
public class Client implements Runnable, ChatComponent {
	
	public static void main(String args[]) throws IOException {
		if (args.length != 2)
			throw new RuntimeException("Syntax: ChatClient <host> <port>");

		Client client = new Client(args[0], Integer.parseInt(args[1]));
					
	}

	protected ObjectInputStream inputStream;

	protected ObjectOutputStream outputStream;
	
	protected Thread thread;
	
	private String name;

	public Client(String host, int port) {
		try {
			intialize(host, port);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public void intialize(String host, int port) throws Exception {
		
			System.out.println("Connecting to " + host + " (port " + port
					+ ")...");
			Socket socket = new Socket(host, port);
			name = socket.getInetAddress().toString() + "("+System.currentTimeMillis()+")";	
			System.out.println("T1");
			this.outputStream = new ObjectOutputStream((socket.getOutputStream()));
			System.out.println("T2");
			this.inputStream = new ObjectInputStream((socket.getInputStream()));
			System.out.println("T3");
			thread = new Thread(this);
			thread.start();
					
	}
	
	public String getName() {
		return name;
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

	/**
	 * decides what to do with incoming messages
	 * 
	 * @param msg
	 *            the message (Object) received from the sockets
	 */
	protected void handleIncomingMessage(Object msg) {
		if (msg instanceof TextMessage) {
			
			//<--CODING MUNGE HAT KEIN "OR" DESWEGEN BLEIBT ES DRIN!
			String tmpValue;
			if ((tmpValue = ((TextMessage)msg).getSetting(Utils.CODING1)) != null) 
				Utils.decode(tmpValue,(TextMessage) msg);
			
			if ((tmpValue = ((TextMessage)msg).getSetting(Utils.CODING2)) != null) 
				Utils.decode(tmpValue,(TextMessage) msg);	
			//--> CODING
			
			fireAddLine(((TextMessage) msg));
		}
	}

	public void send(TextMessage msg) {
		try {
			//<--CODING MUNGE HAT KEIN "OR" DESWEGEN BLEIBT ES DRIN!
			String tmpValue;
			if ((tmpValue = msg.getSetting(Utils.CODING1)) != null) 
				Utils.encode(tmpValue,msg);
			
			if ((tmpValue = msg.getSetting(Utils.CODING2)) != null) 
				Utils.encode(tmpValue,msg);	
			//--> CODING
			
			
			msg.setSender(getName());
			
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
	public void fireAddLine(TextMessage msg) {
		for (Iterator iterator = listeners.iterator(); iterator.hasNext();) {
			ChatLineListener listener = (ChatLineListener) iterator.next();
			listener.newChatLine(msg);
		}
	}

	public void stop() {
		thread.stop();
	}

}
