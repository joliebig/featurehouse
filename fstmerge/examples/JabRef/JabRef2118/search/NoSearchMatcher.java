package net.sf.jabref.search; 

import ca.odell.glazedlists.matchers.Matcher; 

import net.sf.jabref.BibtexEntry; 


public  class  NoSearchMatcher implements  Matcher ,  Matcher<BibtexEntry> {
	
    

	

    


	

    


	
	public static final Matcher<BibtexEntry> INSTANCE = new NoSearchMatcher();

	

	public boolean matches(BibtexEntry object) {
		return true;
	}


}
