


public interface ChatComponent {
	String getName();
	void addLineListener(ChatLineListener listener); 
	void removeLineListener(ChatLineListener listner); 
	void fireAddLine(TextMessage msg);
}