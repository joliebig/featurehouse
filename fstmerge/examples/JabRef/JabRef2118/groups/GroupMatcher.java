package net.sf.jabref.groups; 

import ca.odell.glazedlists.matchers.Matcher; 
import net.sf.jabref.BibtexEntry; 


public  class  GroupMatcher implements  Matcher ,  Matcher<BibtexEntry> {
	

    public static GroupMatcher INSTANCE = new GroupMatcher();

	

    


	

    public boolean matches(BibtexEntry entry) {
        return entry.isGroupHit();
    }


}
