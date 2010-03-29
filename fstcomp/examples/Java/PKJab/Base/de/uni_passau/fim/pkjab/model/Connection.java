
import java.io.IOException;
import java.util.Iterator;
import java.util.Set;

import javax.security.auth.callback.Callback;
import javax.security.auth.callback.CallbackHandler;
import javax.security.auth.callback.NameCallback;
import javax.security.auth.callback.PasswordCallback;
import javax.security.auth.callback.UnsupportedCallbackException;
import javax.security.sasl.Sasl;
import javax.security.sasl.SaslClient;

import org.xml.sax.ContentHandler;
import org.xml.sax.SAXException;

import pkjab.de.uni_passau.fim.pkjab.model.messages.Message;
import pkjab.de.uni_passau.fim.pkjab.model.tags.AbstractXMLTag;
import pkjab.de.uni_passau.fim.pkjab.model.tags.Iq;
import pkjab.de.uni_passau.fim.pkjab.model.xmpp.XMPPAuthHandler;
import pkjab.de.uni_passau.fim.pkjab.model.xmpp.XMPPWriter;
import pkjab.de.uni_passau.fim.pkjab.util.ConnectionState;
import pkjab.de.uni_passau.fim.pkjab.util.Jid;
import pkjab.de.uni_passau.fim.pkjab.util.Observable;
import pkjab.de.uni_passau.fim.pkjab.util.Observer;
import pkjab.de.uni_passau.fim.pkjab.util.UserState;

public class Connection extends Observable {

	protected final ConnectionCallback callback = new ConnectionCallback(this);

	ConnectionState state = ConnectionState.OFFLINE;
	private ConnectionThread thread = null;
	private final Roster roster = new Roster();
	
	Jid jid;
	final String server;
	final int port;
	final String password;
	private UserState userState = UserState.ONLINE;
	
	public Connection(Jid user, String password) {
		this(user, password, user.getDomain(), 5222);
	}
	
	public Connection(Jid user, String password, String server, int port) {
		this.server = server;
		this.port = port;
		this.jid = user;
		this.password = password;
	}
	
	public synchronized void connect() {
		if (!isConnected() && (state == ConnectionState.OFFLINE)) {
			state = ConnectionState.CONNECTING;
			try {
				(thread = new ConnectionThread(this)).start();
			} catch (SAXException e) {
				// no XML parser available
				e.printStackTrace();
				state = ConnectionState.OFFLINE;
				thread = null;
			}
		}
	}
	
	public synchronized void disconnect() {
		if ((state != ConnectionState.OFFLINE) && (state != ConnectionState.DISCONNECTING)) {
			if (state == ConnectionState.ONLINE) {
				try {
					thread.getOutput().sendPresence(UserState.OFFLINE, null);
				} catch (IOException e) { /* do nothing */ }
			}
			state = ConnectionState.DISCONNECTING;
			thread.closeSocket(); // this may result in a call to this method again!
			thread = null;
			state = ConnectionState.OFFLINE;
			for (Iterator it = getRoster().getContacts().iterator(); it.hasNext();) {
				((Contact) it.next()).clear();
			}
			notifyObservers(null);
		}
	}
	
	public synchronized boolean isConnected() {
		if (state == ConnectionState.ONLINE) {
			// check if really still online
			if (!thread.isConnected()) {
				disconnect();
				return false;
			}
			return true;
		}
		return false;
	}
	
	public void setUserState(UserState newState, String text) {
		if (newState == UserState.OFFLINE) {
			disconnect();
		} else {
			userState = newState;
			if (!isConnected()) {
				connect();
			} else {
				try {
					thread.getOutput().sendPresence(userState, text);
					notifyObservers(null);
				} catch (IOException e) {
					disconnect(); 
				}
			}
		}
	}
	
	public UserState getUserState() {
		if (userState == UserState.OFFLINE || userState == null) {
			userState = UserState.ONLINE;
		}
		return isConnected() ? userState : UserState.OFFLINE;
	}
	
	public void sendMessage(Message msg) {
		msg.setFrom(jid);
		try {
			thread.getOutput().sendMessage(msg);
		} catch (IOException e) {
			disconnect();
		}
	}
	
	public Roster getRoster() {
		return roster;
	}

	ConnectionState getState() {
		return state;
	}
	
	public Jid getJid() {
		return jid;
	}
	
	protected void notifyObservers(Message msg) {
		super.notifyObservers(msg);
	}
	
	XMPPWriter getOutput() {
		return thread.getOutput();
	}

	void setReader(ContentHandler handler) {
		thread.setReader(handler);
	}
}
