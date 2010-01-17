package net.sf.jabref.gui;

import net.sf.jabref.BibtexEntry;
import net.sf.jabref.Util;

import java.util.Comparator;


public class IsMarkedComparator implements Comparator {

    public int compare(Object o1, Object o2) {

        BibtexEntry e1 = (BibtexEntry)o1,
                 e2 = (BibtexEntry)o2;

        if (Util.isMarked(e1))
            return Util.isMarked(e2) ? 0 : -1;

        else return Util.isMarked(e2) ? 1 : 0;
    }

}
