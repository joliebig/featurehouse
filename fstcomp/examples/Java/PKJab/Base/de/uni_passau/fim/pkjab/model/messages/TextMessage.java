package de.uni_passau.fim.pkjab.model.messages;

public class TextMessage extends Message {

	private String body = null;
	private String subject = null;
	private String type = null;
	private String thread = null;
	
	public void setType(String t) {
		type = t;
	}
	
	public String getType() {
		return type == null ? "normal" : type;
	}

	public void setSubject(String s) {
		subject = s;
	}

	public String getSubject() {
		return subject;
	}

	public void setBody(String b) {
		body = b;
	}

	public String getBody() {
		return body;
	}

	public void setThread(String t) {
		thread = t;
	}

	public String getThread() {
		return thread;
	}
	
	public String toString() {
		return getBody();
	}
	
	public String toXML() {
		StringBuffer result = new StringBuffer(String.format("<message from='%s' to='%s'", getFrom(), getTo()));
		if (type != null) {
			result.append(" type='");
			result.append(type);
			result.append('\'');
		}
		result.append('>');
		if (subject != null) {
			result.append("<subject>");
			result.append(subject);
			result.append("</subject>");
		}
		if (thread != null) {
			result.append("<thread>");
			result.append(body);
			result.append("</thread>");
		}
		if (body != null) {
			result.append("<body>");
			result.append(body);
			result.append("</body>");
		}
		result.append("</message>");
		return result.toString();
	}
}
