

package org.gjt.sp.jedit.syntax;


import gnu.regexp.RE;
import java.util.*;



public class ParserRuleSet
{
	
	
	public static ParserRuleSet getStandardRuleSet(byte id)
	{
		return standard[id];
	} 

	
	public ParserRuleSet(String modeName, String setName)
	{
		this.modeName = modeName;
		this.setName = setName;
		ruleMapFirst = new ParserRule[RULE_BUCKET_COUNT];
		ruleMapLast = new ParserRule[RULE_BUCKET_COUNT];
	} 

	
	public String getModeName()
	{
		return modeName;
	} 

	
	public String getSetName()
	{
		return setName;
	} 

	
	public String getName()
	{
		return modeName + "::" + setName;
	} 

	
	public Hashtable getProperties()
	{
		return props;
	} 

	
	public void setProperties(Hashtable props)
	{
		this.props = props;
		_noWordSep = null;
	} 

	
	public void addRule(ParserRule r)
	{
		ruleCount++;

		int key = Character.toUpperCase(r.hashChar)
			% RULE_BUCKET_COUNT;
		ParserRule last = ruleMapLast[key];
		if(last == null)
			ruleMapFirst[key] = ruleMapLast[key] = r;
		else
		{
			last.next = r;
			ruleMapLast[key] = r;
		}
	} 

	
	public ParserRule getRules(char ch)
	{
		int key = Character.toUpperCase(ch) % RULE_BUCKET_COUNT;
		return ruleMapFirst[key];
	} 

	
	public int getRuleCount()
	{
		return ruleCount;
	} 

	
	public int getTerminateChar()
	{
		return terminateChar;
	} 

	
	public void setTerminateChar(int atChar)
	{
		terminateChar = (atChar >= 0) ? atChar : -1;
	} 

	
	public boolean getIgnoreCase()
	{
		return ignoreCase;
	} 

	
	public void setIgnoreCase(boolean b)
	{
		ignoreCase = b;
	} 

	
	public KeywordMap getKeywords()
	{
		return keywords;
	} 

	
	public void setKeywords(KeywordMap km)
	{
		keywords = km;
		_noWordSep = null;
	} 

	
	public boolean getHighlightDigits()
	{
		return highlightDigits;
	} 

	
	public void setHighlightDigits(boolean highlightDigits)
	{
		this.highlightDigits = highlightDigits;
	} 

	
	public RE getDigitRegexp()
	{
		return digitRE;
	} 

	
	public void setDigitRegexp(RE digitRE)
	{
		this.digitRE = digitRE;
	} 

	
	public ParserRule getEscapeRule()
	{
		return escapeRule;
	} 

	
	public void setEscapeRule(ParserRule escapeRule)
	{
		addRule(escapeRule);
		this.escapeRule = escapeRule;
	} 

	
	public byte getDefault()
	{
		return defaultToken;
	} 

	
	public void setDefault(byte def)
	{
		defaultToken = def;
	} 

	
	public String getNoWordSep()
	{
		if(_noWordSep == null)
		{
			_noWordSep = noWordSep;
			if(noWordSep == null)
				noWordSep = "";
			if(keywords != null)
				noWordSep += keywords.getNonAlphaNumericChars();
		}
		return noWordSep;
	} 

	
	public void setNoWordSep(String noWordSep)
	{
		this.noWordSep = noWordSep;
		_noWordSep = null;
	} 

	
	
	public boolean isBuiltIn()
	{
		return builtIn;
	} 

	
	public String toString()
	{
		return getClass().getName() + "[" + modeName + "::" + setName + "]";
	} 

	
	private static ParserRuleSet[] standard;

	static
	{
		standard = new ParserRuleSet[Token.ID_COUNT];
		for(byte i = 0; i < standard.length; i++)
		{
			standard[i] = new ParserRuleSet(null,null);
			standard[i].setDefault(i);
			standard[i].builtIn = true;
		}
	}

	private static final int RULE_BUCKET_COUNT = 128;

	private String modeName, setName;
	private Hashtable props;

	private KeywordMap keywords;

	private int ruleCount;

	private ParserRule[] ruleMapFirst;
	private ParserRule[] ruleMapLast;

	private int terminateChar = -1;
	private boolean ignoreCase = true;
	private byte defaultToken;
	private ParserRule escapeRule;

	private boolean highlightDigits;
	private RE digitRE;

	private String _noWordSep;
	private String noWordSep;

	private boolean builtIn;
	
}
