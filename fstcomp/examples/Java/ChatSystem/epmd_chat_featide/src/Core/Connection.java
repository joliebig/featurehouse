


import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;


/**
 * class for an individual connection to a client. allows to send messages to
 * this client and handles incoming messages.
 */
public class Connection implements Runnable {
	protected Socket socket;

	protected ObjectInputStream inputStream;
	protected ObjectOutputStream outputStream;

	protected Server server;
	protected boolean connectionOpen = true;
	protected Thread thread;
	

	
	public Connection(Socket s, Server server) {
		this.socket = s;
		try {
			inputStream = new ObjectInputStream((s.getInputStream()));
			outputStream = new ObjectOutputStream((s.getOutputStream()));
			thread = new Thread(this);
			thread.start();
			
		} catch (IOException e) {
			e.printStackTrace();
		}
		this.server = server;
	}

	public void stop() {
		thread.stop();
	}
	/**
	 * waits for incoming messages from the socket
	 */
	public void run() {
		String clientName = socket.getInetAddress().toString();
		try {
			server.broadcast(new TextMessage(clientName + " has joined."));
			while (connectionOpen) {

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
			server.removeConnection(this);
			server.broadcast(new TextMessage(clientName + " has left."));
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
	protected void handleIncomingMessage(Object msg) {
		if (msg instanceof TextMessage) {
			String tmpContent = ((TextMessage) msg).getContent();			
			server.broadcast(((TextMessage) msg));
	
		}
	}


	public void send(TextMessage msg) {
		
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
