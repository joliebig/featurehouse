

import java.awt.Color;

/**
 * serializable message that can be send over the sockets between client and
 * server. 
 */
public class TextMessage extends ChatMessage {

	private static final long serialVersionUID = 2191809085095630164L;
		
	protected String content;

	public TextMessage(String content) {
		this.content = content;
	}

	public String getContent() {
		return content;
	}
	
	protected void setContent(String content) {
		this.content = content;
	}
}
