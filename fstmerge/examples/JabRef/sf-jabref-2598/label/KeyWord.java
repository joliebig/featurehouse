
package net.sf.jabref.label;

import java.util.HashSet;

public class KeyWord extends HashSet<String> {

	private static KeyWord singleton;

	private KeyWord() {
		
		add("society");
		add("transaction");
		add("transactions");
		add("journal");
		add("review");
		add("revue");
		add("communication");
		add("communications");
		add("letters");
		add("advances");
		add("proceedings");
		add("proceeding");
		add("international");
		add("joint");
		add("conference");
	}
 
	public static KeyWord getKeyWord() {
		if (singleton == null)
			singleton = new KeyWord();
		return singleton;
	}

	public boolean isKeyWord(String matchWord) {
		if (contains(matchWord.toLowerCase())) {
			return true;
		}
		return false;
	}

	public boolean isKeyWordMatchCase(String matchWord) {
		if (contains(matchWord)) {
			return true;
		}
		return false;
	}

}
