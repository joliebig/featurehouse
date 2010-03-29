
public class Subscription {
	
	public static final Subscription NONE = new Subscription("keines");
	public static final Subscription BOTH = new Subscription("beide");
	public static final Subscription TO = new Subscription("nach");
	public static final Subscription FROM = new Subscription("von");
	
	private final String humanName;
	
	private Subscription(final String humanName) {
		this.humanName = humanName;
	}
	
	public String getHumanName() {
		return humanName;
	}
	
	public String toString() {
		return super.toString().toLowerCase();
	}
	
	public static Subscription valueOf(String subscription) {
		subscription = subscription.toUpperCase();
		if (subscription.equals("NONE")) {
			return NONE;
		} else if (subscription.equals("BOTH")) {
			return BOTH;
		} else if (subscription.equals("TO")) {
			return TO;
		} else if (subscription.equals("FROM")) {
			return FROM;
		} else {
			return null;
		}
	}
	
	public Subscription removeTo() {
		if (this == BOTH || this == FROM) {
			return FROM;
		} else if (this == TO || this == NONE) {
			return NONE;
		} else {
			return null;
		}
	}
	
	public Subscription addTo() {
		if (this == BOTH || this == FROM) {
			return BOTH;
		} else if (this == TO || this == NONE) {
			return TO;
		} else {
			return null;
		}
	}
	
	public Subscription removeFrom() {
		if (this == BOTH || this == TO) {
			return TO;
		} else if (this == FROM || this == NONE) {
			return NONE;
		} else {
			return null;
		}
	}
	
	public Subscription addFrom() {
		if (this == BOTH || this == TO) {
			return BOTH;
		} else if (this == FROM || this == NONE) {
			return NONE;
		} else {
			return null;
		}
	}
}
