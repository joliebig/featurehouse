package de.uni_passau.fim.pkjab.util;

class ChatState {
	
	public String toString() {
		if (this == ACTIVE)
			return "ACTIVE";
		else if (this == COMPOSING)
			return "COMPOSING";
		else if (this == INACTIVE)
			return "INACTIVE";
		else if (this == PAUSED)
			return "PAUSED";
		else if (this == GONE)
			return "GONE";
		else 
			throw new Error();
	}		
}
