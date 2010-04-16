

public class Connection {
	
	protected void sendConnectedMessage(String clientName) {
		TextMessage connectedMessage = new TextMessage(clientName + " has joined.");
		
		connectedMessage.setColor(Color.RED);
		connectedMessage.setSender("Server");
		server.broadcast(connectedMessage);
	}
	
	protected void sendDisconnectedMessage(String clientName) {
		TextMessage message = new TextMessage(clientName + " has left.");
		
		message.setColor(Color.RED);
		server.removeConnection(this);
		server.broadcast(message);
	}
}