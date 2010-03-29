

import java.io.IOException;
import java.util.Iterator;
import java.util.Set;

import javax.security.sasl.Sasl;
import javax.security.sasl.SaslClient;

import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;

import pkjab.de.uni_passau.fim.pkjab.model.Connection;
import pkjab.de.uni_passau.fim.pkjab.model.SaslCallbackHandler;
import pkjab.de.uni_passau.fim.pkjab.model.messages.Message;
import pkjab.de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import pkjab.de.uni_passau.fim.pkjab.model.tags.Iq;
import pkjab.de.uni_passau.fim.pkjab.model.xmpp.XMPPAuthHandler;
import pkjab.de.uni_passau.fim.pkjab.util.ConnectionState;
import pkjab.de.uni_passau.fim.pkjab.util.Jid;
import pkjab.de.uni_passau.fim.pkjab.util.Observable;
import pkjab.de.uni_passau.fim.pkjab.util.Observer;
import pkjab.de.uni_passau.fim.pkjab.util.UserState;

	
/* ======================================= callback methods for XMPPReader ====
 * any exceptions thrown here end up wrapped in a SAXException and thrown by
 * XMLReader.parse(), so they are caught in ConnectionThread.run()
 */

public class ConnectionCallback {
	
	public final Connection connection;
	
	ConnectionCallback(Connection connection) {
		this.connection = connection;
	}

	public void setResource(String resource) {
		connection.jid = new Jid(connection.jid.getUser(), connection.jid.getDomain(), resource);
	}
	
	public synchronized void setXMPPReader(final ContentHandler handler) {
		System.out.println("Activating handler " + handler.getClass().getSimpleName());
		connection.setReader(handler);
	}
	
	public synchronized XMPPAuthHandler firstInitWasSuccessful(Set features, Set saslMechanisms) throws IOException {
		System.out.println("firstInitWasSuccessful");
		if (features != null) {
			System.out.println("Supported features: " + features);
		}
		if (connection.state != ConnectionState.DISCONNECTING) {
			// Step 1: TLS would be here
			// Step 2: Authenticate
			if (saslMechanisms == null) {
				System.err.println("Server does not support SASL authentification mechanisms!");
				connection.disconnect();
				return null;
			} else {
				System.out.println("Supported SASL mechanisms: " + saslMechanisms);
				connection.state = ConnectionState.AUTHENTICATING;
				SaslClient sasl = Sasl.createSaslClient((String[])saslMechanisms.toArray(new String[0]),
							null, "xmpp", connection.getJid().getDomain(), null, new SaslCallbackHandler(connection));
				if (sasl == null) {
					System.err.println("No SASL provider found!");
					connection.disconnect();
					return null;
				}
				connection.getOutput().sendAuthInit(sasl.getMechanismName(), 
						sasl.hasInitialResponse() ? sasl.evaluateChallenge(new byte[0]) : null);
				return new XMPPAuthHandler(sasl, connection.getOutput());
			}
		}
		return null;
	}
	
	public synchronized void secondInitWasSuccessful(Set features) throws IOException {
		System.out.println("secondInitWasSuccessful");
		if (features != null) {
			System.out.println("Supported features: " + features);
		}
		if (connection.state != ConnectionState.DISCONNECTING) {
			connection.getOutput().sendBind(connection.getJid().getResource());
			connection.getOutput().sendSessionInit();
			connection.getOutput().sendPresence(connection.getUserState(), null);
		}
	}
	
	public synchronized void handleQuery(Iq iq) throws IOException {
		if (iq.getChild() != null) {
			if (iq.type.equals("result")) {
				if (iq.getChild().getName().equals("bind")) {
					System.out.println("handleQuery: Bind ok");

				} else if (iq.getChild().getName().equals("session")) {
					System.out.println("handleQuery: Session establishment ok");
					connection.state = ConnectionState.ONLINE;
					connection.notifyObservers(null);
				} else {
					System.err.println("handleQuery: Result-Query with unknown child: " + iq.getChild());
				}
			} else {
				System.err.println("handleQuery: " + iq.type + "-Query with unknown child: " + iq.getChild());
			}
		} else {
			System.err.println("handleQuery: " + iq.type + "-Query without child");
		}
	}
}
