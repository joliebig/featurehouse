package de.uni_passau.fim.pkjab.model.tags;

public abstract class AbstractXMLTag implements Comparable {

	public final String uri;
	
	protected AbstractXMLTag(String uri) {
		if (uri == null) {
			throw new IllegalArgumentException();
		}
		this.uri = uri;
	}
	
	public abstract String getName();
	
	public boolean equals(Object o) {
		return (o != null && o instanceof XMLTag) ? equals((XMLTag) o) : false;
	}
	
	public boolean equals(AbstractXMLTag o) {
		return (this == o) || (getName().equals(o.getName()) && uri.equals(o.uri));
	}
	
	public int compareTo(Object o) {
		if (o == null || !(o instanceof AbstractXMLTag)) {
			throw new IllegalArgumentException();
		}
		int temp = getName().compareTo(((AbstractXMLTag)o).getName());
		return temp == 0 ? uri.compareTo(((AbstractXMLTag)o).uri) : temp;
	}
	
	public String toString() {
		return getName() + "(" + uri + ")";
	}
}
