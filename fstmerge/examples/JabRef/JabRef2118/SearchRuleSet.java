
package net.sf.jabref; 

import java.util.*; 
import java.util.regex.PatternSyntaxException; 

import java.util.Enumeration; 
import java.util.Map; 
import java.util.Vector; 

public  class  SearchRuleSet implements  SearchRule {
	
    

	

    public void addRule(SearchRule newRule) {
        ruleSet.add(newRule);
    }


	

    public void clearRules() {
        ruleSet.clear();
    }


	

    


	
    protected Vector<SearchRule> ruleSet = new Vector<SearchRule>();

	

    public int applyRule(Map<String, String> searchString, BibtexEntry bibtexEntry) throws PatternSyntaxException{
        int score = 0;
        Enumeration<SearchRule> e = ruleSet.elements();
        while (e.hasMoreElements()) {
            score += e.nextElement().applyRule(searchString,
                    bibtexEntry);
        }
        return score;
    }


}
