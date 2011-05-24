


public class Connection {
	private String password="EPMD4EVER";
	Boolean auth=false;
	public void handleIncomingMessage(String name, TextMessage msg){
		if(msg instanceof AuthMessage){
			if(msg.getContent().equals(password))
				auth=true;
		}
		
		if(auth)
			original(name,msg);
		else
			send(new TextMessage("Keine Erfolgreiche Authentification"));	
	}
}