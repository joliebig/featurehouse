

public class Server {
	
	protected void acceptConnections(int port) throws IOException {
		connections.add(new HistoryWriter("serverHistory"));
		
		original(port);
	}
}