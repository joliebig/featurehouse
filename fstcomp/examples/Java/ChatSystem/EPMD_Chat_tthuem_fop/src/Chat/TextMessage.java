

import java.io.Serializable;

/**
 * serializable message that can be send over the sockets between client and
 * server. 
 */
public class TextMessage implements Serializable {

	private static final long serialVersionUID = -9161595018411902079L;
	
	protected String content;

	public TextMessage(String content) {
		setContent(content);
	}
	
	public void setContent(String content) {
		this.content = content;
	}

	public String getContent() {
		return content;
	}

}
