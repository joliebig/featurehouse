package net.sf.jabref; 

import java.util.Comparator; 


public  class  MarkedComparator implements  Comparator ,  Comparator<BibtexEntry> {
	

    

	

    public MarkedComparator(Comparator<BibtexEntry> next) {
        this.next = next;
    }


	
    


	

    private int idCompare(BibtexEntry b1, BibtexEntry b2) {
	    return ((String)(b1.getId())).compareTo((String)(b2.getId()));
    }


	

    Comparator<BibtexEntry> next;

	
    public int compare(BibtexEntry e1, BibtexEntry e2) {

        if (e1 == e2)
            return 0;

        boolean mrk1 = Util.isMarked(e1),
                mrk2 = Util.isMarked(e2);

        if (mrk1 == mrk2)
            return (next != null ? next.compare(e1, e2) : idCompare(e1, e2));

        else if (mrk2)
            return 1;
        else return -1;
    }


}
