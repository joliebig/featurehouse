

import java.awt.Color;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

/**
 * class for an individual connection to a client. allows to send messages to
 * this client and handles incoming messages.
 */
public class Connection extends Thread implements ConnectionInterface {
	protected Socket socket;

	protected ObjectInputStream inputStream;

	protected ObjectOutputStream outputStream;

	protected Server server;
	protected boolean connectionOpen = true;

	public Connection(Socket s, Server server) {
		this.socket = s;
		try {
			inputStream = new ObjectInputStream((s.getInputStream()));
			outputStream = new ObjectOutputStream((s.getOutputStream()));
		} catch (IOException e) {
			e.printStackTrace();
		}

		this.server = server;
	}

	protected void sendConnectedMessage(String clientName) {
		TextMessage connectedMessage = new TextMessage(clientName + " has joined.");
			
		connectedMessage.setSender("Server");
		server.broadcast(connectedMessage);
		
		System.out.println(connectedMessage.getContent());
	}
	
	protected void sendDisconnectedMessage(String clientName) {
		TextMessage message = new TextMessage(clientName + " has left.");
			
		server.removeConnection(this);
		server.broadcast(message);
	}
	
	/**
	 * waits for incoming messages from the socket
	 */
	public void run() {
		String clientName = socket.getInetAddress().toString();
		try {
			sendConnectedMessage(clientName);
			
			while (connectionOpen) {
				try {
					Object msg = inputStream.readObject();
					handleIncomingMessage(clientName, msg);
				} catch (ClassNotFoundException e) {
					e.printStackTrace();
				}
			}
		} catch (IOException ex) {
			//ex.printStackTrace();
		} finally {
			sendDisconnectedMessage(clientName);
			try {
				socket.close();
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}
	}

	/**
	 * decides what to do with incoming messages
	 * 
	 * @param name
	 *            name of the client
	 * @param msg
	 *            received message
	 */
	protected void handleIncomingMessage(String name, Object msg) {
		if (msg instanceof TextMessage) {
			((TextMessage)msg).setSender(name);
			server.broadcast((TextMessage)msg);
		}
	}

	public void send(TextMessage msg) {
		try {
			synchronized (outputStream) {
				System.out.println("CONNECTION SEND: " + msg.getContent());
				outputStream.writeObject(msg);
			}
			outputStream.flush();
		} catch (IOException ex) {
			connectionOpen = false;
		}
	}
}
