

import java.awt.Color;

public class EncryptedTextMessage extends TextMessage {

	private static final long serialVersionUID = -7194234609367496025L;

	protected int clearTextLength;
	protected int clearSenderLength;
	
	public EncryptedTextMessage(String content) {
		super(content);
		
		this.clearTextLength = content.length();
		
		setContent(encryptMessage(content));
	}

	public EncryptedTextMessage(String content, Color color) {
		super(content);
		
		this.clearTextLength = content.length();
		
		setContent(encryptMessage(content));
	}
	
	public void setSender(String sender) {
		super.setSender(encryptMessage(sender));
		clearSenderLength = sender.length();
	}
	
	public String getSender(int key) {
		return decryptMessage(super.getSender(), key, clearSenderLength);
	}
	
	public String getContent(int key) {
		return decryptMessage(super.getContent(), key, clearTextLength);
	}
	
	private String encryptMessage(String text) {
		return encryptMessage(text, 13);
	}
	
	protected String encryptMessage(String text, int shift) {
		return text;
	}
	
	protected String decryptMessage(String text, int key, int textLength) {
		return text;
	}
}
