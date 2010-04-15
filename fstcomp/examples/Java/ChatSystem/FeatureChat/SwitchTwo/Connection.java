

public class Connection 
{
	protected void handleIncomingMessage(String name, Object msg) {
		
				
		if (msg instanceof AuthMessage) //check if it is a password message
		{
			
			if (SwitchTwo.crypto(((AuthMessage) msg).getContent()).equals(this.server.getPassword()))
			{
				AuthMessage m = new AuthMessage("Server",SwitchTwo.crypto("OK"));
				this.send(m);
			} 
			else 
			{
				
				server.removeConnection(this);
				content = "Authentication failed!! User is kicked.";
				server.broadcast("Server" ,SwitchTwo.crypto(content) ,Color.orange);

				try {
					socket.close();
				} catch (IOException ex) {
					ex.printStackTrace();
				}
			}
		}
		
				
		if (msg instanceof TextMessage)
		{	
			content = ((TextMessage) msg).getSender() + ": " + SwitchTwo.crypto(((TextMessage) msg).getContent());
			server.broadcast(((TextMessage) msg).getSender(), ((TextMessage) msg).getContent() ,((TextMessage) msg).getColor());	
		
		}
			
	}
	
	public void run() {
		
		String clientName = socket.getInetAddress().toString();
		server.broadcast("Server",SwitchTwo.crypto("New User has joined."),Color.orange);
		
		try {
			
			while (connectionOpen) {

				try {
					Object msg = inputStream.readObject();
					handleIncomingMessage(clientName, msg);
										
				} catch (ClassNotFoundException e) 
				{
					e.printStackTrace();
				}
				

			}
		} catch (IOException ex) {
			ex.printStackTrace();
		} finally {
			server.removeConnection(this);
			server.broadcast("Server",SwitchTwo.crypto("User has left."),Color.orange);
			

			try {
				socket.close();
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}
	}
}