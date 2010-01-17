package net.sf.jabref.search; 

import net.sf.jabref.BibtexEntry; 

import ca.odell.glazedlists.matchers.Matcher; 


public  class  NoSearchMatcher implements  Matcher<BibtexEntry> {
	
	public static final Matcher<BibtexEntry> INSTANCE = new NoSearchMatcher();

	

	public boolean matches(BibtexEntry object) {
		return true;
	}



}
