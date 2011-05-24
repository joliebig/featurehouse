package de.uni_passau.fim.pkjab.model.tags;

import org.xml.sax.Attributes;

public class Iq extends AbstractXMLTag {
	public final String type;
	public final String id;
	public final String from;
	public final String to;
	
	private AbstractXMLTag child = null;
	private AbstractXMLTag error = null;
	
	public Iq(final String uri, final String type, final String id,
			final String from, final String to) {
		super(uri);
		this.type = type;
		this.id = id;
		this.from = from;
		this.to = to;
	}

	public Iq(final String uri, final Attributes atts) {
		super(uri);
		this.type = atts.getValue("", "type");
		this.id = atts.getValue("", "id");
		this.from = atts.getValue("", "from");
		this.to = atts.getValue("", "to");
	}
	
	public void setChild(final AbstractXMLTag child) {
		if (this.child == null) {
			this.child = child;
		}
	}

	public AbstractXMLTag getChild() {
		return child;
	}

	public void setError(final AbstractXMLTag error) {
		if (this.error == null && type.equals("error")) {
			this.error = error;
		}
	}

	public AbstractXMLTag getError() {
		return error;
	}

	public String getName() {
		return "iq";
	}
}
