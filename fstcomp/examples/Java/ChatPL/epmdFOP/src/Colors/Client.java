

public class Client {

	protected void propagateMessage(TextMessage msg) {
		fireAddLine(getMessageContent(msg) + "\n", msg.getColor());
	}
	
	public void fireAddLine(String line, Color color) {
		for (Iterator iterator = listeners.iterator(); iterator.hasNext();) {
			ChatLineListener listener = (ChatLineListener) iterator.next();
			listener.newChatLine(line, color);
		}
	}
	
	public void send(String line, Color color) {
		TextMessage msg = new TextMessage(line);
		msg.setColor(color);
		original(msg);
	}
}