

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
	
	protected void init(String host, int port) {
		original(host, port);
		new Console(this);
	}

}
