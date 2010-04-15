

public class Connection {

	ReverseEncryption crypt = new ReverseEncryption();
	
	protected void handleIncomingMessage(String name, String msg) {
		original(name, crypt.getDecryptedMessage(msg));
	}

	public void send(String line) {
		original(crypt.getEncryptedMessage(line));
	}
}