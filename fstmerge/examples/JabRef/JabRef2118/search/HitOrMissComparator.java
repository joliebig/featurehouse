package net.sf.jabref.search; 

import java.util.Comparator; 
import ca.odell.glazedlists.matchers.Matcher; 

import net.sf.jabref.BibtexEntry; 


public  class  HitOrMissComparator implements  Comparator ,  Comparator<BibtexEntry> {
	
    

	

    public HitOrMissComparator(Matcher<BibtexEntry> hitOrMiss) {
        this.hitOrMiss = hitOrMiss;
    }


	

    


	
    private Matcher<BibtexEntry> hitOrMiss;

	

    public int compare(BibtexEntry o1, BibtexEntry o2) {
        if (hitOrMiss == null)
            return 0;
        
        boolean
                hit1 = hitOrMiss.matches(o1),
                hit2 = hitOrMiss.matches(o2);
        if (hit1 == hit2)
            return 0;
        else
            return hit1 ? -1 : 1;
    }


}
