
public final class Jid implements Comparable {

	private final String user;
	private final String domain;
	private final String resource;
	
	// caching
	private String bareJid = null;
	private String qualifiedJid = null;
	
	public Jid(String user, String domain, String resource) {
		if (user == null || domain == null) {
			throw new IllegalArgumentException();
		}
		this.user = user;
		this.domain = domain;
		this.resource = resource;
	}
	
	public boolean isQualified() {
		return resource != null;
	}

	public String getBareJid() {
		if (bareJid == null) {
			bareJid = user + "@" + domain;
		}
		return bareJid;
	}
	
	public String getQualifiedJid() {
		if (qualifiedJid == null) {
			qualifiedJid = isQualified() ? user + "@" + domain + "/" + resource : null;
		}
		return qualifiedJid;
	}
	
	public String toString() {
		return isQualified() ? getQualifiedJid() : getBareJid();
	}
	
	public String getUser() {
		return user;
	}

	public String getDomain() {
		return domain;
	}

	public String getResource() {
		return resource;
	}
	
	public boolean equals(Object o) {
		return (o instanceof Jid) ? equals((Jid) o) : false;
	}
	
	public boolean equals(Jid o) {
		return (o != null) 
			&& (user == o.user || user.equalsIgnoreCase(o.user))
			&& (domain == o.domain || domain.equalsIgnoreCase(o.domain))
			&& (resource == o.resource || (resource != null && resource.equalsIgnoreCase(o.resource)));
	}
	
	public boolean equals(String o) {
		return toString().equalsIgnoreCase(o);
	}
	
	public int compareTo(Object o) {
		if (o != null && o instanceof Jid) {
			return toString().compareToIgnoreCase(o.toString());
		} else {
			return -1;
		}
	}
	
	public int compareTo(String o) {
		return toString().compareToIgnoreCase(o);
	}
	
	public static Jid fromString(String jid) {
		if (jid == null) {
			return null;
		}
		String[] parts = jid.split("/");
		if (parts.length > 2) {
			return null;
		}
		String resource = (parts.length == 2) ? parts[1] : null;
		
		parts = parts[0].split("@");
		if (parts.length != 2) {
			return null;
		}
		
		Jid result = new Jid(parts[0], parts[1], resource);
		if (result.isQualified()) {
			result.qualifiedJid = jid;
		} else {
			result.bareJid = jid;
		}
		return result;
	}
}
