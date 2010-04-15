

public class Connection {
	protected boolean authentificated = false;

	public void run() {
		String clientName = socket.getInetAddress().toString();
		try {			
			while (connectionOpen) {
				try {
					Object msg = inputStream.readObject();
					handleIncomingMessage(clientName, msg);
				} catch (ClassNotFoundException e) {
					e.printStackTrace();
				}
			}
		} catch (IOException ex) {
			//ex.printStackTrace();
		} finally {
			sendDisconnectedMessage(clientName);
			try {
				socket.close();
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}
	}

	protected void handleIncomingMessage(String name, Object msg) {
		if (msg instanceof TextMessage && authentificated) {
			original(name, msg);
		}
		
		if (msg instanceof ConnectionMessage) {
			ConnectionMessage connectionMessage = (ConnectionMessage)msg;
			
			if (connectionMessage.getPassword() == server.getPasswordHash()) {
				this.authentificated = true;
				
				sendConnectedMessage(socket.getInetAddress().toString());
			} else {
				this.server.removeConnection(this);
				this.connectionOpen = false;
				try {
					socket.close();
				} catch (IOException e) {
					//e.printStackTrace();
				}
			}
		}
	}
	
}