


import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;


public class Connection extends Thread{
	protected Socket socket;
	protected ObjectInputStream inputStream;
	protected ObjectOutputStream outputStream;
	private Server server;
	private boolean connectionOpen = true;

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
		try {
			server.broadcast(new TextMessage(clientName+ " has joined."));
			while (connectionOpen) {

				try {
					Object msg = inputStream.readObject();
					if(msg instanceof TextMessage)
						handleIncomingMessage(clientName,(TextMessage) msg);
				} catch (ClassNotFoundException e) {
					e.printStackTrace();
				}

			}
		} catch (IOException ex) {
			ex.printStackTrace();
		} finally {
			server.removeConnection(this);
			server.broadcast(new TextMessage(clientName+ "has left."));
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
	public void handleIncomingMessage(String name, TextMessage msg) {
			msg.setContent(name+ " - "+ msg.getContent());
			server.broadcast(msg);
		
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