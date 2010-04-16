
import java.awt.Color;

public class Client {
	protected Color textColor = Color.BLACK;
	public void send(String line){
		TextMessage msg = new TextMessage(line);
		msg.setColor(textColor);
		send(msg);
	}
	
}