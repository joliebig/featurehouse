
import pkjab.de.uni_passau.fim.pkjab.model.messages.Message;

/**
 * A class has to implement this interface if it wants to be informed of changes in observable objects.
 * This class is a simple variant of the java.util.Observer.
 * @see java.util.Observer
 * @see Observable
 * @author Alex
 */
public interface Observer {

	/**
	 * This method is called whenever the observed object is changed. 
	 */
	void update(Message msg);

}
