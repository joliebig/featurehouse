package de.uni_passau.fim.pkjab.util;

public class UserState {

	public static final UserState ONLINE = new UserState("Online");
	public static final UserState AWAY = new UserState("Abwesend");
	public static final UserState XA = new UserState("Nicht verfuegbar"); 
	public static final UserState DND = new UserState("Nicht stoeren");
	public static final UserState CHAT = new UserState("Frei fuer Chat");
	public static final UserState OFFLINE = new UserState("Offline");
	
	private final String humanText;
	
	private UserState(final String humanText) {
		this.humanText = humanText;
	}
	
	public String toString() {
		return humanText;
	}
	
	public static UserState valueOf(String userState) {
		userState = userState.toUpperCase();
		if (userState.equals("ONLINE")) {
			return ONLINE;
		} else if (userState.equals("AWAY")) {
			return AWAY;
		} else if (userState.equals("XA")) {
			return XA;
		} else if (userState.equals("DND")) {
			return DND;
		} else if (userState.equals("CHAT")) {
			return CHAT;
		} else if (userState.equals("OFFLINE")) {
			return OFFLINE;
		} else {
			throw new IllegalArgumentException();
		}
	}
	
	
	public String toXML() {
		if (this == OFFLINE) {
			return "unavailable";
		} else {
			return super.toString().toLowerCase();
		}
	}
}
