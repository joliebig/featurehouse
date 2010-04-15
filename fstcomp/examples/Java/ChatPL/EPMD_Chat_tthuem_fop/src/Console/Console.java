
public class Console implements ChatLineListener {
	
	public Console(Client chatClient) {
		// register listener so that we are informed whenever a new chat message
		// is received (observer pattern)
		chatClient.addLineListener(this);
	}

	public void newChatLine(String line) {
		System.out.println(line);
	}

}
