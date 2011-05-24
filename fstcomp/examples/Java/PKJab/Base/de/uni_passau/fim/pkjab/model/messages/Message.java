package de.uni_passau.fim.pkjab.model.messages;

import de.uni_passau.fim.pkjab.util.Jid;

public abstract class Message {

	private Jid from = null;
	private Jid to = null;
	
	public void setFrom(Jid source) {
		from = source;
	}

	public Jid getFrom() {
		return from;
	}

	public void setTo(Jid target) {
		to = target;
	}

	public Jid getTo() {
		return to;
	}	
	
	public abstract String toXML();
}
