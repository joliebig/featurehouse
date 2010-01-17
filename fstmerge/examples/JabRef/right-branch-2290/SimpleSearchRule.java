
package net.sf.jabref;

import net.sf.jabref.export.layout.format.RemoveBrackets;

import java.util.*;

public class SimpleSearchRule implements SearchRule {

    final boolean m_caseSensitiveSearch;
    static RemoveBrackets removeBrackets = new RemoveBrackets();

    public SimpleSearchRule(boolean caseSensitive) {
        m_caseSensitiveSearch = caseSensitive;
    }

    public int applyRule(Map searchStrings, BibtexEntry bibtexEntry) {
        String searchString = (String) searchStrings.values().iterator().next();

        if (!m_caseSensitiveSearch)
            searchString = searchString.toLowerCase();
        int score = 0;
        int counter = 0;
        Object fieldContentAsObject;
        String fieldContent;
        Object[] fields = bibtexEntry.getAllFields();
        for (int i = 0; i < fields.length; i++) {
            fieldContentAsObject = bibtexEntry.getField(fields[i].toString()); 
            if (fieldContentAsObject != null)
                try {
                    fieldContent = removeBrackets.format(fieldContentAsObject.toString());
                    if (!m_caseSensitiveSearch)
                        fieldContent = fieldContent.toLowerCase();
                    counter = fieldContent.indexOf(searchString, counter);
                    while (counter >= 0) {
                        ++score;
                        counter = fieldContent.indexOf(searchString, counter + 1);
                    }
                } catch (Throwable t) {
                    System.err.println("sorting error: " + t);
                }
            counter = 0;
        }
        return score;
    }

}
