

import java.awt.Color;


public class Console implements ChatListener
{
	private Client chatClient;
	
	public Console(Client chatClient)
	{
		chatClient.addListener(this);
		
		this.chatClient = chatClient;
	}
	
	public void newChatLine(String sender,String line, Color color) {

		System.out.print(sender.concat(line));
		
	
	}	
}