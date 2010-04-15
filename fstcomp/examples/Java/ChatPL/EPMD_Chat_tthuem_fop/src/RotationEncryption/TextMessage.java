

import java.io.Serializable;

/**
 * serializable message that can be send over the sockets between client and
 * server. 
 */
public class TextMessage {

	private static final int KEY = 13;
	
	public void setContent(String content) {
		original(rotate(content, KEY));
	}

	public String getContent() {
		return rotate(original(), -KEY);
	}

	private String rotate(String text, int key) {
		char[] chars = text.toCharArray();
		for (int i = 0; i < chars.length; i++)
			chars[i] = (char) (chars[i] + key);
		return new String(chars);
	}

}
