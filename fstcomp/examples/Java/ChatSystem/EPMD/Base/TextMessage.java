

import java.io.Serializable;

public class TextMessage implements Serializable {

	private String content;

	public TextMessage(String content) {
		super();
		this.content = content;
	}

	public String getContent() {
		return content;
	}

}