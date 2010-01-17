
package net.sf.jabref;

import net.sf.jabref.export.layout.format.RemoveBrackets;

import java.util.*;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import java.util.regex.Matcher;

public class RegExpRule implements SearchRule {

    final boolean m_caseSensitiveSearch;
    static RemoveBrackets removeBrackets = new RemoveBrackets();

    public RegExpRule(boolean caseSensitive) {
        m_caseSensitiveSearch = caseSensitive;
    }

    public int applyRule(Map searchStrings, BibtexEntry bibtexEntry) throws PatternSyntaxException {

        int score = 0;
        Iterator e = searchStrings.values().iterator();

        String searchString = (String) e.next();

        int flags = 0;
        if (!m_caseSensitiveSearch)
            flags = Pattern.CASE_INSENSITIVE; 
        
        Pattern pattern = Pattern.compile(searchString, flags);

        Object[] fields = bibtexEntry.getAllFields();
        score += searchFields(fields, bibtexEntry, pattern);

        return score;
    }

    protected int searchFields(Object[] fields, BibtexEntry bibtexEntry,
                               Pattern pattern) {
        int score = 0;
        if (fields != null) {
            for (int i = 0; i < fields.length; i++) {
                try {
                    Object value = bibtexEntry.getField((String)fields[i]);
                    if (value != null) {
                        Matcher m = pattern.matcher(removeBrackets.format((String)value));
                        if (m.find())
                            score++;
                    }
                }

                catch (Throwable t) {
                    System.err.println("Searching error: " + t);
                }
            }
        }
        return score;
    }

}
