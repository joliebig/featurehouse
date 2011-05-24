


import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashSet;
import java.util.Iterator;

public class Server {
	public static void main(String args[]) throws IOException {
		if (args.length != 1)
			throw new RuntimeException("Syntax: ChatServer <port>");      
        new Server(Integer.parseInt(args[0]));
	}
	
	/**
	 * awaits incoming connections and creates Connection objects accordingly.
	 *
	 * @param port
	 *            port to listen on
	 */	
	public Server(int port) throws IOException {
	ServerSocket server = new ServerSocket(port);
	init();
	while (true) {
			System.out.println("Waiting for Connections...");
			Socket client = server.accept();
			System.out.println("Accepted from " + client.getInetAddress());
			Connection c = connectTo(client);
			c.start();
	}
	}
	/**
	 * creates a new connection for a socket
	 *
	 * @param socket
	 *            socket
	 * @return the Connection object that handles all further communication with
	 *         this socket
	 */
	public Connection connectTo(Socket socket) {
		Connection connection = new Connection(socket, this);
		connections.add(connection);
		return connection;
	}
	/**
	 * list of all known connections
	 */
	protected HashSet connections = new HashSet();

	/**
	 * send a message to all known connections
	 *
	 * @param text
	 *            content of the message
	 */
	
	  public void broadcast(TextMessage msg){	
		synchronized (connections) {
			for (Iterator iterator = connections.iterator(); iterator.hasNext();) {
				Connection connection = (Connection) iterator.next();
				connection.send(msg);
			}
		}
	}

	/**
	 * remove a connection so that broadcasts are no longer sent there.
	 *
	 * @param connection
	 *            connection to remove
	 */
	public void removeConnection(Connection connection) {
		connections.remove(connection);
	}
	
	/**
	 * Diese Methode wird aus dem Kontruktor aufgerufen und spaeter verfeinert
	 *
	*/
	public void init(){
	}
	
}