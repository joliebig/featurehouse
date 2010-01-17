
package net.sf.jabref.groups;

import java.util.*;

import net.sf.jabref.*;


class AndOrSearchRuleSet extends SearchRuleSet {

    private boolean and, invert;

    public AndOrSearchRuleSet(boolean and, boolean invert) {
        this.and = and;
        this.invert = invert;
    }

    public int applyRule(Map searchString, BibtexEntry bibtexEntry) {
        int score = 0;
        Enumeration e = ruleSet.elements();

        
        while (e.hasMoreElements()) {
            score += (((SearchRule) e.nextElement()).applyRule(searchString,
                    bibtexEntry) > 0 ? 1 : 0);
        }

        
        
        boolean res;
        if (and)
            res = (score == ruleSet.size());
        else
            res = (score > 0);

        if (invert)
            return (res ? 0 : 1);
        return (res ? 1 : 0);
    }
}
