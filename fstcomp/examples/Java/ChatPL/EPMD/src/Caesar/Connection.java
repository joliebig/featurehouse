

public class Connection {

	CaesarEncryption crypt = new CaesarEncryption();
	
	protected void handleIncomingMessage(String name, String msg) {
		original(name, crypt.getDecryptedMessage(msg));
	}

	public void send(String line) {
		original(crypt.getEncryptedMessage(line));
	}
}