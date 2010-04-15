

import java.io.Serializable;

public abstract class ChatMessage implements Serializable {
	
	private static final long serialVersionUID = -303081935883850900L;
	
	private String sender;
	
	public String getSender() {
		return sender;
	}
	
	public void setSender(String sender) {
		this.sender = sender;
	}
}
