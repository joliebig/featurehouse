

public class Client 
{
	protected void handleIncomingMessage(Object msg) {
		if (msg instanceof AuthMessage)
		{
			gui.checkPassword(SwitchTwo.crypto(((AuthMessage) msg).getContent()));
		}
		if (msg instanceof TextMessage) 
		{
			fireAddLine(((TextMessage) msg).getSender() + ": ", SwitchTwo.crypto(((TextMessage) msg).getContent())
						+ "\n", ((TextMessage) msg).getColor());
			

		}
	}
	
	public void send(String from, String line, Color color) 
	{
		line = SwitchTwo.crypto(line);
		original(from, line, color);
	}
}