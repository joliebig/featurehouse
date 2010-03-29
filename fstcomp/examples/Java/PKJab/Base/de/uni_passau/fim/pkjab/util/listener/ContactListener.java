
import java.util.EventListener;

public interface ContactListener extends EventListener {
	
	public void contactStateChanged(ContactStateEvent e);
	
	public void messageReceived(MessageReceivedEvent e);

}
