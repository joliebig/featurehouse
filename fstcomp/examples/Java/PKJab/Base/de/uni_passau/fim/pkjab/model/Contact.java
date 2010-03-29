
import java.util.Collection;
import java.util.EventListener;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import javax.swing.event.EventListenerList;

import pkjab.de.uni_passau.fim.pkjab.model.messages.Message;
import pkjab.de.uni_passau.fim.pkjab.model.messages.TextMessage;
import pkjab.de.uni_passau.fim.pkjab.util.Jid;
import pkjab.de.uni_passau.fim.pkjab.util.Subscription;
import pkjab.de.uni_passau.fim.pkjab.util.UserState;
import pkjab.de.uni_passau.fim.pkjab.util.listener.ContactListener;
import pkjab.de.uni_passau.fim.pkjab.util.listener.ContactStateEvent;
import pkjab.de.uni_passau.fim.pkjab.util.listener.MessageReceivedEvent;

public class Contact implements Comparable {

	protected EventListenerList listeners = new EventListenerList();

	protected final Connection connection;
	private final Jid jid;
	private final Map resources = new TreeMap(); 

	private String nick;
	private Subscription subscription = Subscription.NONE;

	public Contact(final Connection connection, final Jid jid) {
		if (jid.isQualified()) {
			this.jid = new Jid(jid.getUser(), jid.getDomain(), null);
		} else {
			this.jid = jid;
		}
		this.connection = connection;
		this.nick = jid.getBareJid();
		fireStateChanged();
	}

	public TextMessage sendMessage(String text) {
		TextMessage msg = new TextMessage();
		msg.setTo(jid);
		msg.setBody(text);
		connection.sendMessage(msg);
		return msg;
	}

	public UserState getState() {
		Resource resource = null;
		for (Iterator it = resources.values().iterator(); it.hasNext(); ) {
			Resource r = (Resource) it.next();
			if ((resource == null || r.getPriority() > resource.getPriority())
					&& (r.getState() != UserState.OFFLINE)) {
				resource = r;
			}
		}
		return (resource == null) ? UserState.OFFLINE : resource.getState();
	}

	public String getStateText() {
		Resource resource = null;
		for (Iterator it = resources.values().iterator(); it.hasNext(); ) {
			Resource r = (Resource) it.next();
			if ((resource == null || r.getPriority() > resource.getPriority())
					&& (r.getState() != UserState.OFFLINE)) {
				resource = r;
			}
		}
		return (resource == null) ? null : resource.getText();
	}
	
	public void addResource(Resource resource) {
		resources.put(resource.getResource(), resource);
		fireStateChanged();
	}
	
	public Resource getResource(String resource) {
		return (Resource) (resources.get(resource));
	}
	
	public Resource removeResource(String resource) {
		return (Resource) resources.remove(resource);
	}
	
	protected Collection getResources() {
		return resources.values();
	}
	
	public String getNick() {
		return nick;
	}

	public void setNick(String n) {
		if (n == null || n.equals("")) {
			nick = jid.getBareJid();
		} else {
			nick = n;
		}
	}
	
	public Jid getBareJid() {
		return jid;
	}
	
	public void clear() {
		UserState oldState = getState();
		resources.clear();
		if (oldState != UserState.OFFLINE) {
			fireStateChanged();
		}		
	}
	
	public void setMessageReceived(Message msg) {
		fireMessageReceived(new MessageReceivedEvent(this, msg));
	}	
	
	// listener pattern
	
	 public void addContactListener(ContactListener listener) {
		listeners.add(ContactListener.class, listener);
	}

	public void removeContactListener(ContactListener listener) {
		listeners.remove(ContactListener.class, listener);
	} 

	public synchronized void fireStateChanged() {
		ContactStateEvent e = new ContactStateEvent(this, getState());
		EventListener[] listener = listeners.getListeners(ContactListener.class);
		for (int i = 0; i < listener.length; i++ )
			((ContactListener) listener[i]).contactStateChanged(e);
	}
	
	private synchronized void fireMessageReceived(MessageReceivedEvent e) {
		EventListener[] listener = listeners.getListeners(ContactListener.class);
		for (int i = 0; i < listener.length; i++ )
			((ContactListener) listener[i]).messageReceived(e);
	}

	public boolean equals(final Object o) {
		return (this == o)
		|| ((o != null && o instanceof Contact) ? jid.equals(((Contact)o).getBareJid()) : false);
	}
	
	public int compareTo(final Object o) {
		if (o == null || !(o instanceof Contact)) {
			throw new IllegalArgumentException();
		}
		return jid.compareTo(((Contact)o).getBareJid());
	}

	public Subscription getSubscription() {
		return subscription;
	}

	public void setSubscription(final Subscription newSubscription) {
		if (newSubscription == null) {
			subscription = Subscription.NONE;
		} else {
			subscription = newSubscription;
		}
	}
	
	public String toString() {
		return nick + "(" + jid + ")";
	}
}
