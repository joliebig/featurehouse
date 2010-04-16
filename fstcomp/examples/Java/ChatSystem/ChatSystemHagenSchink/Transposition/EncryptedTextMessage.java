

public class EncryptedTextMessage {

	protected String encryptMessage(String text, int shift) {
		int pos = 1;
				
		if ((shift % text.length()) != 0) {
			pos = shift % text.length();
		}
		text = transposition(text, pos);
		
		return original(text, shift);
	}
	
	protected String decryptMessage(String text, int key, int textLength) {
		text = transpositionReverse(text, key % textLength);
		
		while (text.length() > 0 && text.charAt(text.length() - 1) == ' ') {
			text = text.substring(0, text.length() - 1);
		}
		
		return text;
	}

	private String transposition(String message, int shift) {
		String encryption = "";
		
		while (shift != 1 && shift != 0 && message.length() % shift != 0) {
			message += " ";
		}		
		
		for (int start=0; start <= shift - 1; start++) {
			int pos = start;
			
			while (pos < message.length()) {
				encryption += message.charAt(pos);
				pos += shift;
			}
			
			if ((pos - shift) == message.length() - 1) {
				break;
			}
		}
		
		return encryption;
	}
	
	private String transpositionReverse(String message, int shift) {
		if (shift != 1 && shift > 0) {
			shift = message.length() / shift;
		}
		
		return transposition(message, shift);
	}
}