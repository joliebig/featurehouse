
package net.sf.jabref; 

import net.sf.jabref.export.layout.format.RemoveBrackets; 

import java.util.*; 
import java.util.regex.Pattern; 
import java.util.regex.PatternSyntaxException; 
import java.util.regex.Matcher; 

import java.util.Map; 
import java.util.Set; 

public  class  RegExpRule implements  SearchRule {
	

    final boolean m_caseSensitiveSearch;

	
    static RemoveBrackets removeBrackets = new RemoveBrackets();

	

    public RegExpRule(boolean caseSensitive) {
        m_caseSensitiveSearch = caseSensitive;
    }


	

    


	

    


	

    public int applyRule(Map<String, String> searchStrings, BibtexEntry bibtexEntry) throws PatternSyntaxException {

        int score = 0;
        String searchString = searchStrings.values().iterator().next();

        int flags = 0;
        if (!m_caseSensitiveSearch)
            flags = Pattern.CASE_INSENSITIVE; 
        
        Pattern pattern = Pattern.compile(searchString, flags);

        score += searchFields(bibtexEntry.getAllFields(), bibtexEntry, pattern);

        return score;
    }

	

    protected int searchFields(Set<String> fields, BibtexEntry bibtexEntry,
                               Pattern pattern) {
        int score = 0;
        if (fields != null) {
        	for (String field : fields){
                try {
                    Object value = bibtexEntry.getField(field);
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
