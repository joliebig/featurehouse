

import java.io.Serializable;

/**
 * serializable message that can be send over the sockets between client and
 * server. 
 */
public class TextMessage {

	public void setContent(String content) {
		original(backwards(content));
	}

	public String getContent() {
		return backwards(original());
	}

	private String backwards(String text) {
		char[] chars = new char[text.length()];
		for (int i = 0; i < text.length(); i++)
			chars[i] = text.charAt(text.length() - i - 1);
		return new String(chars);
	}

}
