


import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.LinkedList;

public class Client {
	protected Socket socket;
	protected BufferedReader in;
	protected PrintWriter out;
	
	
	private final static String HOST = "127.0.0.1";
	
	public Client() throws UnknownHostException, IOException {
		socket = new Socket(HOST, Server.SERVER_PORT);
		in = new BufferedReader(
			    new InputStreamReader(
			    socket.getInputStream()));
		out = new PrintWriter(socket.getOutputStream(), true);
	}
	
	protected boolean sendCommand(String command, String parameter) throws IOException {
		send("<" + command + ">" + parameter);
		String line = read();
		System.out.println("? " + line + " == " + line.equals(ChatProtocol.OK_STRING));
		System.out.flush();
		if (line.equals(ChatProtocol.OK_STRING))
			return true;
		return false;
	}
	
	public boolean authenticate(String username, String password) throws IOException {
		return sendCommand("auth", username + ChatProtocol.USERNAME_PASSWORD_SEP + password);
	}
	
	public String getMessage() throws IOException {
		send("<get>");
		String result = read();
		if (result.startsWith(ChatProtocol.MESSAGE_PREFIX))
			return result.substring(ChatProtocol.MESSAGE_PREFIX.length());
		return null;
	}
	
	public boolean sendMessage(String msg) throws IOException {
		return sendCommand("send", msg);
	}
	
	protected void send(String s) {
		System.out.println(" > sent " + s);
		System.out.flush();
		out.println(s);
	}
	
	protected String read() throws IOException {
		String line = in.readLine().replace("\\n", "\n");
		System.out.println(" > read " + line);
		System.out.flush();
		return line;
	}
	
		
}
