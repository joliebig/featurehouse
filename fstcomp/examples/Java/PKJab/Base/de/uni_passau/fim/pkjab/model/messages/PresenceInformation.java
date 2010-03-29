

public class PresenceInformation extends Message {

	private String type = null;

	public void setType(String type) {
		this.type = type;
	}
	
	public String getType() {
		return type;
	}

	public String toString() {
		return "User " + getFrom() + " " + type;
	}
	
	public String toXML() {
		throw new Error();
	}
}
