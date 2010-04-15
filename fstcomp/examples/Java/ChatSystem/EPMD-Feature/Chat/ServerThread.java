

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class ServerThread extends Thread {

	private final Socket socket;
	
	
	public ServerThread(Socket s) {
		super("ServerThread");
		this.socket = s;
	}
	
	public void run() {
		try {
		    PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
		    BufferedReader in = new BufferedReader(
					    new InputStreamReader(
					    socket.getInputStream()));

		    String inputLine, outputLine;
		    ChatProtocol kkp = new ChatProtocol();

		    while ((inputLine = in.readLine()) != null) {
		    	try {
					outputLine = kkp.processInput(inputLine);
				} catch (ProtocolException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
					continue;
				}
		    	if (kkp.isQuit())
		    		break;
		    	
		    	out.println(outputLine);
		    }
		    
		    out.close();
		    in.close();
		    socket.close();
		} catch (IOException e) {
		    e.printStackTrace();
		}
	}
}
