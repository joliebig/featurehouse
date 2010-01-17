
package net.sf.jabref.label;

import java.util.Hashtable;

import net.sf.jabref.BibtexDatabase;
import net.sf.jabref.BibtexEntry;
import net.sf.jabref.BibtexEntryType;
import net.sf.jabref.Util;



public class LabelMaker {

    public BibtexEntry applyRule(BibtexEntry newEntry, BibtexDatabase base){
	String newKey = "";
        if(ruleTable.containsKey(newEntry.getType().getName())){
            newKey = ruleTable.get(newEntry.getType().getName()).applyRule(newEntry) ;
        }
        else{
		newKey = applyDefaultRule(newEntry) ;
        }

	
	newKey = Util.checkLegalKey(newKey);

	
	if (base.setCiteKeyForEntry(newEntry.getId(), newKey)) {
	    
	    char c = 'b';
	    String modKey = newKey+"a";
	    while (base.setCiteKeyForEntry(newEntry.getId(), modKey))
		modKey = newKey+((char)(c++));	    
	}

	
	

		return newEntry ;
    }

    public void setDefaultRule(LabelRule newRule) {
		defaultRule = newRule ;
    }

    public String applyDefaultRule(BibtexEntry newEntry) {
        return defaultRule.applyRule(newEntry) ;
    }


    
    public void addRule(LabelRule rule,BibtexEntryType type){
       ruleTable.put(type.getName(),rule) ;
    }

    protected LabelRule defaultRule = new ArticleLabelRule() ;
    protected Hashtable<String, LabelRule> ruleTable = new Hashtable<String, LabelRule>() ;

}


