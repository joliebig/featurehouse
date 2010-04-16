

public class Client {
	
	protected String getMessageContent(TextMessage msg) {
		TextMessage message = (TextMessage)msg;
		String text = "";
		
		if (msg instanceof EncryptedTextMessage) {
			EncryptedTextMessage eMessage = (EncryptedTextMessage)message;
			text += eMessage.getSender(13) + " wrote : ";
			text += eMessage.getContent(13);
		} else {
			text += message.getSender() + " wrote : ";
			text += message.getContent();
		}
		
		return text;
	}
	
	public void send(String line) {
		EncryptedTextMessage msg = new EncryptedTextMessage(line);
		
		send(msg);
	}
}