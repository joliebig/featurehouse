

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashSet;
import java.util.Iterator;

import javax.swing.JOptionPane;

/**
 * server's main class. accepts incoming connections and allows broadcasting
 */
public class Server {
	int serverPort;
	
	public static void main(String args[]) throws IOException {
		new Server();
	}

	/**
	 * awaits incoming connections and creates Connection objects accordingly.
	 * 
	 * @param port
	 *            port to listen on
	 */
	public Server() throws IOException {
		getConfiguration();
		ServerSocket server = new ServerSocket(serverPort);
		while (true) {
			System.out.println("Waiting for Connections...");
			Socket client = server.accept();
			System.out.println("Accepted from " + client.getInetAddress());
			Connection c = connectTo(client);
			c.start();
		}
	}
	public Server(boolean validate){}
		
	
	protected synchronized void getConfiguration()
	{
		try
		{
			char[] buffer = new char[128];

			FileReader configFile = new FileReader("serverConfig.cfg");
			
			configFile.read( buffer );
			
			String value = String.copyValueOf( buffer );
			String[] temp = value.split(";");
			
			serverPort = Integer.parseInt( temp[0] );
			configFile.close();
		}
		catch( FileNotFoundException fnf_e )
		{
			JOptionPane.showMessageDialog( null,"Configuration File Not Found, Using Defaults","Configuration File Missing",JOptionPane.ERROR_MESSAGE );
			serverPort = 1665;
	
		}
		catch( IOException io_e )
		{
			JOptionPane.showMessageDialog( null,"Error Reading Configuration File, Using Defaults","Configuration Error",JOptionPane.ERROR_MESSAGE );
			serverPort = 1665;
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
	public void broadcast(TextMessage msg) {
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

}
