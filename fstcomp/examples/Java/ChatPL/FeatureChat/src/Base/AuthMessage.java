

public class AuthMessage extends Message {

	private static final long serialVersionUID = -9161595018411902079L;
	private String content;
	private String sender;
		
	public AuthMessage(String sender, String content) {
		super();
		this.content = content;
		this.sender = sender;
	}

	public String getContent() {
		return this.content;
	}
	
	public String getSender() { 
		return this.sender; 
	}
	
	
}