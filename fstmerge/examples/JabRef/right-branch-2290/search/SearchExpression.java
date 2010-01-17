

package net.sf.jabref.search;

import antlr.RecognitionException;
import antlr.TokenStreamException;
import antlr.collections.AST;
import java.io.StringReader;
import java.util.*;
import java.util.Hashtable;
import java.util.regex.PatternSyntaxException;
import net.sf.jabref.BibtexEntry;
import net.sf.jabref.JabRefPreferences;
import net.sf.jabref.SearchRule;

public class SearchExpression implements SearchRule {
	private SearchExpressionTreeParser treeParser = new SearchExpressionTreeParser();
	private AST ast = null;
	private JabRefPreferences prefs = null;
	public SearchExpression(JabRefPreferences prefs, Hashtable searchOptions)
		throws TokenStreamException, RecognitionException, PatternSyntaxException
	{
		this.prefs = prefs;
		
		SearchExpressionParser parser = new SearchExpressionParser(new SearchExpressionLexer(new StringReader(
			searchOptions.elements().nextElement().toString()))); 
		parser.caseSensitive = this.prefs.getBoolean("caseSensitiveSearch");
		parser.regex = this.prefs.getBoolean("regExpSearch");
		parser.searchExpression(); 
		ast = parser.getAST(); 
	}
	public int applyRule(Map searchStrings, BibtexEntry bibtexEntry) {
		try {
			return treeParser.apply(ast,bibtexEntry);
		} catch (RecognitionException e) {
			return 0; 
		} 
	}
}

