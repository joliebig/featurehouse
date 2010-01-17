

package net.sf.jabref; 

import java.util.Comparator; 


public  class  CrossRefEntryComparator implements  Comparator<BibtexEntry> {
	

	private String crossRefField = "crossref";

	

	public int compare(BibtexEntry e1, BibtexEntry e2)
		throws ClassCastException {

		Object f1 = e1.getField(crossRefField), f2 = e2.getField(crossRefField);

		if ((f1 == null) && (f2 == null))
			return 0; 
		if ((f1 != null) && (f2 != null))
			return 0; 
		if (f1 != null)
			return -1;
		else
			return 1;
	}



}
