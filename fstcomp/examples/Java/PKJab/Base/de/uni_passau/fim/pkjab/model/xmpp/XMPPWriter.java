
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import pkjab.de.uni_passau.fim.pkjab.model.messages.Message;
import pkjab.de.uni_passau.fim.pkjab.util.Base64;
import pkjab.de.uni_passau.fim.pkjab.util.UserState;

public class XMPPWriter {

	private static final String CLOSE_EMPTY_TAG = "/>";
	
	private static final String CONNECTION_INIT = "<?xml version='1.0' ?><stream:stream to='%s' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' version='1.0'>";
	private static final String CONNECTION_CLOSE = "</stream:stream>";
	private static final String AUTH_START = "<auth mechanism='%s' xmlns='urn:ietf:params:xml:ns:xmpp-sasl'";
	private static final String AUTH_RESPONSE = "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'";
	private static final String BIND = "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'";
	private static final String BIND_RESOURCE = "><resource>%s</resource></bind>";
	private static final String SESSION_INIT = "<session xmlns='urn:ietf:params:xml:ns:xmpp-session'/>";
	private static final String PRESENCE = "<presence";
	private static final String PRESENCE_TYPE = PRESENCE + " type='%s'";
	private static final String PRESENCE_SHOW = PRESENCE + "><show>%s</show";
	private static final String PRESENCE_STATUS = "><status>%s</status";
	private static final String PRESENCE_CLOSE = "></presence>";
	
	private final BufferedWriter writer;
	private int myId = 0;
	
	public XMPPWriter(OutputStream outputStream) throws IllegalArgumentException, IOException {
		if (outputStream == null) {
			throw new IllegalArgumentException("Socket required.");
		}
		
		writer = new BufferedWriter(new OutputStreamWriter(outputStream));
	}
	
	public synchronized void sendInit(String domain) throws IOException {
		writer.write(String.format(CONNECTION_INIT, domain));
		writer.flush();
	}
	
	public synchronized void sendAuthInit(String mechanism, byte[] initialResponse) throws IOException {
		writer.write(String.format(AUTH_START, mechanism));
		if (initialResponse == null) {
			writer.write(CLOSE_EMPTY_TAG);
		} else {
			writer.write(">");
			writer.write(Base64.encodeBytes(initialResponse));
			writer.write("</auth>");
		}
		writer.flush();
	}

	public synchronized void sendAuthResponse(byte[] response) throws IOException {
		writer.write(AUTH_RESPONSE);
		if (response == null) {
			writer.write(CLOSE_EMPTY_TAG);
		} else {
			writer.write(">");
			writer.write(Base64.encodeBytes(response));
			writer.write("</response>");
		}
		writer.flush();
	}
	
	public synchronized void sendBind(String resource) throws IOException {
		if (resource == null || resource.equals("")) {
			sendIq("set", BIND + CLOSE_EMPTY_TAG, null, null, null);
		} else {
			sendIq("set", BIND + String.format(BIND_RESOURCE, resource), null, null, null);
		}
	}
	
	public synchronized void sendSessionInit() throws IOException {
		sendIq("set", SESSION_INIT, null, null, null);
	}
	
	public synchronized void getRoster() throws IOException {
		sendIqQuery("get", "jabber:iq:roster", "", null, null, null);
	}

	public synchronized void sendPresence(UserState state, String text) throws IOException {
		if (state == UserState.OFFLINE) {
			writer.write(String.format(PRESENCE_TYPE, state.toXML()));
		} else if (state == UserState.ONLINE) {
			writer.write(PRESENCE);
		} else {
			writer.write(String.format(PRESENCE_SHOW, state.toXML()));
		}
		
		if (text != null) {
			writer.write(String.format(PRESENCE_STATUS, text));
		}
		writer.write(PRESENCE_CLOSE);
		writer.flush();
	}
	
	public synchronized void sendQuit() throws IOException {
		// close tags, send quit
		writer.write(CONNECTION_CLOSE);
		writer.flush();
	}
	
	public synchronized void writeRaw(String text) throws IOException {
		writer.write(text);
		writer.flush();
	}
	
	public synchronized void sendIq(String type, String content, String id,
			String to, String from) throws IOException {
		StringBuffer sb = new StringBuffer("<iq id='");
		if (id == null)
			sb.append(myId++);
		else
			sb.append(id);
		sb.append("'");
		
		if (to != null) {
			sb.append(" to='"); sb.append(to); sb.append('\'');
		}
		
		if (from != null) {
			sb.append(" from='"); sb.append(from); sb.append('\'');
		}
		
		sb.append(" type='"); sb.append(type); sb.append("'>");

		sb.append(content);
		sb.append("</iq>");

		writer.write(sb.toString());
		writer.flush();
	}
		
	public synchronized void sendMessage(Message msg) throws IOException {
		writer.write(msg.toXML());
		writer.flush();
	}
	
	public synchronized void sendIqQuery(String type, String namespace,
			String content, String id, String to, String from) throws IOException {
		sendIq(type, String.format("<query xmlns='%s'>%s</query>", namespace, content), id, to, from);
	}
}
