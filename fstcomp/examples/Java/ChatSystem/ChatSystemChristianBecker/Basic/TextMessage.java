

import java.io.Serializable;

public class TextMessage implements Serializable {

	private static final long serialVersionUID = -9161595018411902079L;
	// Inhalt der TextMessage
	private String content;

	public TextMessage(String content) {
		super();
		this.content = content;
	}

	public String getContent() {
		return content;
	}
	
	public void setContent(String content){
		this.content=content;
	}
}