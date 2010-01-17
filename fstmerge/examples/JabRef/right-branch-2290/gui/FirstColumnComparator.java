package net.sf.jabref.gui;

import net.sf.jabref.BibtexEntry;
import net.sf.jabref.Util;

import java.util.Comparator;


public class FirstColumnComparator implements Comparator {

    public int compare(Object o1, Object o2) {

        BibtexEntry e1 = (BibtexEntry)o1,
                 e2 = (BibtexEntry)o2;

        int score1=0, score2=0;

        
        

        
        

        if (e1.hasAllRequiredFields())
            score1++;

        if (e2.hasAllRequiredFields())
            score2++;

        return score1-score2;
    }

}
