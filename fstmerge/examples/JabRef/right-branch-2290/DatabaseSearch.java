
package net.sf.jabref;

import java.util.*;
import java.util.regex.PatternSyntaxException;

import javax.swing.SwingUtilities;

public class DatabaseSearch extends Thread {
	BasePanel panel = null;
	BibtexDatabase thisDatabase = null;
	SearchRuleSet thisRuleSet = null;
	Hashtable thisSearchOptions = null;
	String searchValueField = null;
	boolean reorder, select, grayOut;
	ErrorMessageDisplay errorDisplay;
	Set matches = new HashSet();
	public DatabaseSearch(ErrorMessageDisplay errorDisplay,
			Hashtable searchOptions, SearchRuleSet searchRules,
			BasePanel panel, String searchValueField, boolean reorder,
			boolean grayOut, boolean select) {
		this.panel = panel;
		this.errorDisplay = errorDisplay;
		thisDatabase = panel.getDatabase();
		thisSearchOptions = searchOptions;
		thisRuleSet = searchRules;
		this.searchValueField = searchValueField;
		this.reorder = reorder;
		this.select = select;
		this.grayOut = grayOut;
	}

	public void run() {
		int searchScore = 0;
		matches.clear();
		BibtexEntry bes = null;
		int hits = 0;

		for (Iterator i = thisDatabase.getKeySet().iterator(); i.hasNext();) {
			

			bes = thisDatabase.getEntryById((String) i.next());
			if (bes == null)
				continue;
			

			
			try {
				searchScore = thisRuleSet.applyRule(thisSearchOptions, bes);
			} catch (PatternSyntaxException ex) {
				
				errorDisplay.reportError("Malformed regular expression", ex);
				return;
			}
			
			
			
			if (searchScore > 0)
				searchScore = 1;

			
			bes.setField(searchValueField, String.valueOf(searchScore));

			if (searchScore > 0) {
				hits++;
				matches.add(bes);
			}
		}
		final int outputHits = hits;
		SwingUtilities.invokeLater(new Thread() {
			public void run() {
				panel.output(Globals
                    .lang("Searched database. Global number of hits")
                    + ": " + outputHits);
			}
		});
	}
	
	public Iterator matches() {
		return matches.iterator();
	}
}
