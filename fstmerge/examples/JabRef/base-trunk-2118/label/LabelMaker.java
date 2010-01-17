
package net.sf.jabref.label;

import net.sf.jabref.*;
import java.util.Hashtable ;



public class LabelMaker {

    public BibtexEntry applyRule(BibtexEntry newEntry, BibtexDatabase base){
	String newKey = "";
        if(ruleTable.containsKey(newEntry.getType().getName())){
            newKey = ((LabelRule)ruleTable.get(newEntry.getType().getName())).applyRule(newEntry) ;
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
    protected Hashtable ruleTable = new Hashtable() ;

}


