package net.sf.jabref;

import java.util.Comparator;


public class IdComparator implements Comparator {

    public int compare(Object o1, Object o2) {
        BibtexEntry one = (BibtexEntry)o1,
                two = (BibtexEntry)o2;
        return one.getId().compareTo(two.getId());
    }
}
