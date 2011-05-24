

public class Client 
{
	protected void handleIncomingMessage(Object msg) {
		if (msg instanceof AuthMessage)
		{
			gui.checkPassword(Reverse.crypto(((AuthMessage) msg).getContent()));
		}
		if (msg instanceof TextMessage) 
		{
			fireAddLine(((TextMessage) msg).getSender() + ": ", Reverse.crypto(((TextMessage) msg).getContent())
						+ "\n", ((TextMessage) msg).getColor());
			

		}
	}
	
	public void send(String from, String line, Color color) 
	{
		line = Reverse.crypto(line);
		original(from, line, color);
	}
}