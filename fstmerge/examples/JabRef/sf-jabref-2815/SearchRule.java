
package net.sf.jabref;

import java.util.Map;

public interface SearchRule{

    public int applyRule(Map<String, String> searchStrings,BibtexEntry bibtexEntry) ;

}


