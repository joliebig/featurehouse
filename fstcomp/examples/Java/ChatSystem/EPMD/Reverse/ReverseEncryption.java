

public class ReverseEncryption {

	public String getEncryptedMessage(String msg) {
		StringBuilder builder = new StringBuilder();
		for (int i = 0, size = msg.length(); i < size; ++i)
			builder.append(msg.charAt(size - i - 1));
		return builder.toString();
		
	}

	public String getDecryptedMessage(String msg) {
		return getEncryptedMessage(msg);
	}
}