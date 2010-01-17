package net.sf.jabref.search;

import net.sf.jabref.BibtexEntry;

import java.util.Hashtable;

import ca.odell.glazedlists.matchers.Matcher;


public class SearchMatcher implements Matcher {

        public static SearchMatcher INSTANCE = new SearchMatcher();

        public boolean matches(Object object) {
            BibtexEntry entry = (BibtexEntry)object;
            return entry.isSearchHit();
        }
}
