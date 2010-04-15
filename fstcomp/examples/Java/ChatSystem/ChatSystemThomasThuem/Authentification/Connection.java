


import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;


/**
 * class for an individual connection to a client. allows to send messages to
 * this client and handles incoming messages.
 */
public class Connection {
	/**
	 * decides what to do with incoming messages
	 * 
	 * @param name
	 *            name of the client
	 * @param msg
	 *            received message
	 */
	protected void handleIncomingMessage(String name, Object msg) {
		if (msg instanceof String && "Hossa".equals(msg))
			activated = true;
		if (!activated)
			return;
		original(name, msg);
	}
}
