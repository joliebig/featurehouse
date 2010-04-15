



import java.awt.event.*;
import java.awt.*;
import java.io.*;


public class ConsoleUI implements ChatLineListener{
	protected Client chatClient;
	protected Console myConsole;
	public ConsoleUI(Client chatClient) {
		myConsole = System.console();
		if (myConsole == null){
			System.err.println("Unable to obtain console");
		}
		myConsole.printf("%s\n", "Connected to Server ...");
		this.chatClient = chatClient;
		chatClient.addLineListener(this);
	}
	public void newChatLine(TextMessage msg) {
		String line = msg.getContent();
		myConsole.printf("%s\n", line);
		
	}
}