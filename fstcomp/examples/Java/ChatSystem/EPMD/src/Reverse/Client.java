

public class Client {
	
	ReverseEncryption crypt = new ReverseEncryption();
	
	public void send(String msg) {
		original(crypt.getEncryptedMessage(msg));		
	}
	
	public void fireAddLine(String line) {
		original(crypt.getDecryptedMessage(line));
	}
}