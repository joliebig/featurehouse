
import java.io.*;

public class TextMessage implements Serializable {

	private static final long serialVersionUID = -9161595018411902079L;
	protected String content;

	public TextMessage(String content) {
		super();
		this.content = content;
	}
	public void setContent(String newContent){
		content = newContent;
	}

	public String getContent() {
		return content;
	}
}