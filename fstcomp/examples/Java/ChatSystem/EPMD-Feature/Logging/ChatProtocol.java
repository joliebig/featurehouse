

public class ChatProtocol {
	private final static Logger log = new Logger("server");
	
	protected void postMessageSend(String username, String value) {
		try {
			log.log(username + " : " + value);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public String preProcess(String inputLine) {
		String s = original(inputLine);
		try {
			log.log(s);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return s;
	}
}