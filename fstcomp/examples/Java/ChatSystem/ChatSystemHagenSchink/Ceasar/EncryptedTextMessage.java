

public class EncryptedTextMessage {

	protected String encryptMessage(String text, int shift) {
		int pos = 1;
		
		text = cesar(text, shift);
		
		return original(text, shift);
	}
	
	protected String decryptMessage(String text, int key, int textLength) {		
		text = cesarReverse(text, key);
		
		return text;
	}

	private String cesar(String message, int shift) {

		String encryption = "";
		
		for (int c=0; c < message.length(); c++) {
			encryption += (char)((((int)message.charAt(c)) + 127 + shift) % 127);
		}
		
		return encryption;
	}

	private String cesarReverse(String message, int shift) {
		return cesar(message, -shift);
	}
}