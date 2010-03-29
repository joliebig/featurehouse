
import java.io.IOException;
import java.net.Socket;

import org.xml.sax.ContentHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

import pkjab.de.uni_passau.fim.pkjab.model.xmpp.XMPPReader;
import pkjab.de.uni_passau.fim.pkjab.model.xmpp.XMPPReaderLogin;
import pkjab.de.uni_passau.fim.pkjab.model.xmpp.XMPPWriter;
import pkjab.de.uni_passau.fim.pkjab.util.ConnectionState;

class ConnectionThread extends Thread {

	private final Connection connection;
	private Socket socket = null;
	private XMPPWriter output = null;
	private final XMLReader reader;
	
	/**
	 * @param connection
	 * @throws SAXException 
	 */
	ConnectionThread(Connection connection) throws SAXException {
		this.connection = connection;
		reader = XMLReaderFactory.createXMLReader();
	}

	synchronized void closeSocket() {
		if (socket != null) {
			if (!socket.isClosed()) {
				try {
					output.sendQuit();
				} catch (IOException e) { /* do nothing */ }
				try {
					socket.close();
				} catch (IOException e) { /* do nothing */ }
			}
			socket = null;
		}
	}
	
	synchronized boolean isConnected() {
		return (socket != null) && socket.isConnected();
	}
	
	XMPPWriter getOutput() {
		return output;
	}
	
	synchronized void setReader(final ContentHandler handler) {
		reader.setContentHandler(handler);
	}
	
	public void run() {
		try {
			// connect
			socket = new Socket(connection.server, connection.port);
			socket.setKeepAlive(true);
			reader.setContentHandler((new XMPPReaderLogin(connection.callback)));
			output = new XMPPWriter(socket.getOutputStream());
		
			while (true) {
				output.sendInit(connection.getJid().getDomain());
				
				try {
					reader.parse(new InputSource(socket.getInputStream()));

				} catch (RestartStreamException e) {
					System.out.println("Restarting Stream...");
					reader.setContentHandler((new XMPPReader(connection.callback)));
					continue;
				}
			}
		} catch (IOException e) {
			ConnectionState state = connection.getState();

			if (!(state == ConnectionState.OFFLINE || state == ConnectionState.DISCONNECTING)) {
				System.err.println(e.getMessage());
//				e.printStackTrace();
				connection.disconnect();
			}
		} catch (Exception e) {
			System.err.println(e.getMessage());
//			e.printStackTrace();
			connection.disconnect();
		}
	}
}