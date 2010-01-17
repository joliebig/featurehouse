package net.sf.jabref;

import java.util.Comparator;
import java.util.List;
import java.util.Iterator;


public class FieldComparatorStack implements Comparator {

    List comparators;

    public FieldComparatorStack(List comparators) {
        this.comparators = comparators;
    }

    public int compare(Object o1, Object o2) {
        for (Iterator i=comparators.iterator(); i.hasNext();) {
            int res = ((Comparator)i.next()).compare(o1, o2);
            if (res != 0)
                return res;
        }
        return 0;
    }

}
