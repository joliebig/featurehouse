

public class CaesarEncryption {

	int shift = 13;

	public String getEncryptedMessage(String msg) {
		StringBuilder builder = new StringBuilder();
		for (int i = 0, size = msg.length(); i < size; ++i)
			builder.append((char) (msg.charAt(i) + shift));
		return builder.toString();
	}

	public String getDecryptedMessage(String msg) {
		StringBuilder builder = new StringBuilder();
		for (int i = 0, size = msg.length(); i < size; ++i)
			builder.append((char) (msg.charAt(i) - shift));
		return builder.toString();
	}

}