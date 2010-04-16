

public class Connection {

	private boolean loggedIn = false;
	private String password = "PASSWORD";
	
	protected void handleIncomingMessage(String name, String msg) {
		if (loggedIn) {
			original(name, msg);	

			//return;
		} else {
			if (msg.equals(password)) {
				loggedIn = true;
			}			
		} 
	}
	
	public void send(String line) {
		if (loggedIn) {
			original(line);	
		}		
	}
}