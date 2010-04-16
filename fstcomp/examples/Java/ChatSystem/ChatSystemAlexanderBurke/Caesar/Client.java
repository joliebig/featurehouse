

public class Client {
	
	CaesarEncryption crypt = new CaesarEncryption();
	
	public void send(String msg) {
		original(crypt.getEncryptedMessage(msg));
	}
	
	public void fireAddLine(String line) {
		original(crypt.getDecryptedMessage(line));
	}
}