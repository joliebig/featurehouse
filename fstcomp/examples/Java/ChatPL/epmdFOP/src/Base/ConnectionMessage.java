

public class ConnectionMessage extends ChatMessage {

	private static final long serialVersionUID = 7587374152378187929L;
	private int password;
	
	public ConnectionMessage(String password) {
		this.password = password.hashCode();
	}

	public int getPassword() {
		return password;
	}
}
