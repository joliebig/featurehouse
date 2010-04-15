

public class ClientGUI {
	private Logger log;
	
	public void initGUI() throws Exception {
		log = new Logger("client-" + clientName);
		original();
	}
	
	public String postMessageReceived(String newMessage) {
		if (newMessage.length() > 0)
			try {
				log.log(newMessage);
			} catch(IOException e) {
				e.printStackTrace();
			}
		return original(newMessage);
	}
}