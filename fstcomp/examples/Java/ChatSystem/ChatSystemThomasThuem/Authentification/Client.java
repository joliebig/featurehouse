

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Iterator;


/**
 * simple chat client
 */
public class Client implements Runnable {
	
	/**
	 * main method. waits for incoming messages.
	 */
	public void run() {
		try {
			outputStream.writeObject("Hossa");
			outputStream.flush();
			original();
		} catch (IOException ex) {
			ex.printStackTrace();
		} finally {
			thread = null;
			try {
				outputStream.close();
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}
	}

}
