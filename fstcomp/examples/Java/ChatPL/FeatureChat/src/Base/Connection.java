

import java.awt.Color;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;


/**
 * class for an individual connection to a client. allows to send messages to
 * this client and handles incoming messages.
 */
public class Connection extends Thread {
	protected Socket socket;

	protected ObjectInputStream inputStream;

	protected ObjectOutputStream outputStream;
	
	

	protected Server server;
	protected String content;
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

	/**
	 * waits for incoming messages from the socket
	 */
	public void run() {
		
		String clientName = socket.getInetAddress().toString();
		server.broadcast("Server","New User has joined." ,Color.orange);

		try {
			
			while (connectionOpen) {

				try {
					Object msg = inputStream.readObject();
					handleIncomingMessage(clientName, msg);
										
				} catch (ClassNotFoundException e) 
				{
					e.printStackTrace();
				}
				

			}
		} catch (IOException ex) {
			ex.printStackTrace();
		} finally {
			server.removeConnection(this);
			server.broadcast("Server","User has left.",Color.orange);

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
		
		
		
		
		if (msg instanceof AuthMessage) //check if it is a password message
		{
			
			if (((AuthMessage) msg).getContent().equals(this.server.getPassword()))
			{
				AuthMessage m = new AuthMessage("Server","OK");
				this.send(m);
			} 
			else 
			{
				
				server.removeConnection(this);
				content = "Authentication failed!! User is kicked.";
				server.broadcast("Server" ,content ,Color.orange);

				try {
					socket.close();
				} catch (IOException ex) {
					ex.printStackTrace();
				}
			}
		}
		
				
		if (msg instanceof TextMessage)
		{	
			content = ((TextMessage) msg).getSender() + ": " + ((TextMessage) msg).getContent();
			server.broadcast(((TextMessage) msg).getSender(), ((TextMessage) msg).getContent() ,((TextMessage) msg).getColor());	
		
		}
			
	}

	/**
	 * sends a message to the client
	 * 
	 * @param line
	 *            text of the message
	 */
	public void send(String from, String line, Color c) {
		send(new TextMessage(from, line, c));
	}

	public void send(Message msg) {
		try {
			synchronized (outputStream) {
				outputStream.writeObject(msg);
			}
			outputStream.flush();
		} catch (IOException ex) {
			connectionOpen = false;
		}

	}
	

}