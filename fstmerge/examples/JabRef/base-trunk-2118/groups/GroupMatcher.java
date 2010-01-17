package net.sf.jabref.groups;

import ca.odell.glazedlists.matchers.Matcher;
import net.sf.jabref.BibtexEntry;


public class GroupMatcher implements Matcher {

    public static GroupMatcher INSTANCE = new GroupMatcher();

    public boolean matches(Object object) {
        BibtexEntry entry = (BibtexEntry)object;
        return entry.isGroupHit();
    }
}

