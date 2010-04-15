

public class Client {
	protected void handleIncomingMessage(Object msg) {
		if (msg instanceof TextMessage) {
			String content = ((TextMessage)msg).getContent();
			int contentIndex = content.indexOf("-") + 1;
			String prefix = content.substring(0,contentIndex);
			content = content.substring(contentIndex + 1,content.length());
			String decodedContent = Encryption.encrptMethod2(Encryption.encrptMethod1(content));
			((TextMessage)msg).setContent(prefix + " " + decodedContent);
			fireAddLine((TextMessage)msg);
		}
	}
}