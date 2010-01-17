
package net.sf.jabref;

import java.util.*;
import java.util.regex.PatternSyntaxException;

public class SearchRuleSet implements SearchRule {
    protected Vector ruleSet = new Vector();

    public void addRule(SearchRule newRule) {
        ruleSet.add(newRule);
    }

    public void clearRules() {
        ruleSet.clear();
    }

    public int applyRule(Map searchString, BibtexEntry bibtexEntry) throws PatternSyntaxException{
        int score = 0;
        Enumeration e = ruleSet.elements();
        while (e.hasMoreElements()) {
            score += ((SearchRule) e.nextElement()).applyRule(searchString,
                    bibtexEntry);
        }
        return score;
    }
}
