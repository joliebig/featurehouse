

import java.awt.Color;

public class TextMessage extends Message {

	private static final long serialVersionUID = -9161595018411902079L;
	private String content;
	private String sender;
	private Color color;
		
	public TextMessage(String sender, String content, Color color) {
		super();
		this.content = content;
		this.color =  color;	
		this.sender = sender;
		
	}

	public String getContent() {
		return this.content;
	}
	
	public String getSender() {
		return this.sender;
	}
	
	public Color getColor() {
		return this.color;
	}
	
		
	
}