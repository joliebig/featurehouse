



import java.io.IOException;
import java.net.ServerSocket;

public class Server {

	public final static int SERVER_PORT = 5721;
	/**
	 * @param args
	 * @throws IOException 
	 */
	public static void main(String[] args) throws IOException {
		ServerSocket srvSock = null;
		
		try {
			srvSock = new ServerSocket(SERVER_PORT);
		} catch (IOException e) {
			System.err.println("could not bind to port " + SERVER_PORT);
			e.printStackTrace(System.err);
			System.exit(-1);
		}
		
		boolean listening = true;
		while (listening)
			new ServerThread(srvSock.accept()).start();
		
		srvSock.close();

	}

}
