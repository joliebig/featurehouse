

public class Server {
	
	History history;
	
	public void initServer(int port) {
		original(port);
		history = new History("Server.txt");	
	}
	
	public void broadcast(String text) {
		history.onMessageReceived(text);		
		original(text);
	}

}