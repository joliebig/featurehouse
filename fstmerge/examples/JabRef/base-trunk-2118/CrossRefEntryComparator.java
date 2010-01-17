

package net.sf.jabref;

import java.util.Comparator;


public class CrossRefEntryComparator implements Comparator {

    private String crossRefField = "crossref";

    public int compare(Object o1, Object o2) throws ClassCastException {
	if (!(o1 instanceof BibtexEntry) || !(o2 instanceof BibtexEntry))
	   throw new ClassCastException("Trouble comparing objects. This shouldn't happen.");
	BibtexEntry e1 = (BibtexEntry)o1,
	    e2 = (BibtexEntry)o2;

	Object f1 = e1.getField(crossRefField),
	    f2 = e2.getField(crossRefField);

	if ((f1 == null) && (f2 == null)) return 0; 
	if ((f1 != null) && (f2 != null)) return 0; 
	if (f1 != null) return -1;
	else return 1;
    }

}
