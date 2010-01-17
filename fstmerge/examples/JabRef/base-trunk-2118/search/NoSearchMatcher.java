package net.sf.jabref.search;

import ca.odell.glazedlists.matchers.Matcher;


public class NoSearchMatcher implements Matcher {
    public static final Matcher INSTANCE = new NoSearchMatcher();

    private NoSearchMatcher() {
        
    }

    public boolean matches(Object object) {
        return true;
    }
}
