

package org.gjt.sp.jedit.syntax;


import java.util.*;
import java.util.regex.Pattern;



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
		ruleMap = new HashMap<Character, List<ParserRule>>();
		imports = new ArrayList<ParserRuleSet>();
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

	
	public Hashtable<String, String> getProperties()
	{
		return props;
	} 

	
	public void setProperties(Hashtable<String, String> props)
	{
		this.props = props;
		_noWordSep = null;
	} 

	
	
	public void resolveImports()
	{
		for (ParserRuleSet ruleset : imports)
		{
			if (!ruleset.imports.isEmpty())
			{
				
				ruleset.imports.remove(this);
				ruleset.resolveImports();
			}

			for (List<ParserRule> rules : ruleset.ruleMap.values())
			{
				for (ParserRule rule : rules)
				{
					addRule(rule);
				}
			}

			if (ruleset.keywords != null)
			{
				if (keywords == null)
					keywords = new KeywordMap(ignoreCase);
				keywords.add(ruleset.keywords);
			}
		}
		imports.clear();
	} 

	
	
	public void addRuleSet(ParserRuleSet ruleset)
	{
		imports.add(ruleset);
	} 

	
	public void addRule(ParserRule r)
	{
		ruleCount++;
		Character[] keys;
		if (null == r.upHashChars)
		{
			keys = new Character[1];
			if ((null == r.upHashChar) || (0 >= r.upHashChar.length()))
			{
				keys[0] = null;
			}
			else
			{
				keys[0] = Character.valueOf(r.upHashChar.charAt(0));
			}
		}
		else
		{
			keys = new Character[r.upHashChars.length];
			int i = 0;
			for (char upHashChar : r.upHashChars)
			{
				keys[i++] = upHashChar;
			}
		}
		for (Character key : keys)
		{
			List<ParserRule> rules = ruleMap.get(key);
			if (null == rules)
			{
				rules = new ArrayList<ParserRule>();
				ruleMap.put(key,rules);
			}
			int ruleAmount = rules.size();
			rules.add(r);
			
			if (ruleAmount > 0)
			{
				rules.get(ruleAmount).next = r;
			}
		}
	} 

	
	
	@Deprecated
	public ParserRule getRules(char ch)
	{
		List<ParserRule> rules = getRules(Character.valueOf(ch));
		return rules.get(0);
	} 

	
	public List<ParserRule> getRules(Character key)
	{
		List<ParserRule> rulesForNull = ruleMap.get(null);
		boolean emptyForNull = (rulesForNull == null) || (rulesForNull.size() == 0);
		Character upperKey = null == key ? null : Character.valueOf(Character.toUpperCase(key.charValue()));
		List<ParserRule> rulesForKey = null == upperKey ? null : ruleMap.get(upperKey);
		boolean emptyForKey = (rulesForKey == null) || (rulesForKey.size() == 0);
		if (emptyForNull && emptyForKey)
		{
			return Collections.emptyList();
		}
		else if (emptyForKey)
		{
			return rulesForNull;
		}
		else if (emptyForNull)
		{
			return rulesForKey;
		}
		else
		{
			int size = rulesForNull.size() + rulesForKey.size();
			ArrayList<ParserRule> mixed = new ArrayList<ParserRule>(size);
			mixed.addAll(rulesForKey);
			mixed.addAll(rulesForNull);
			
			rulesForKey.get(rulesForKey.size() - 1).next = rulesForNull.get(0);
			return mixed;
		}
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

	
	public Pattern getDigitRegexp()
	{
		return digitRE;
	} 

	
	public void setDigitRegexp(Pattern digitRE)
	{
		this.digitRE = digitRE;
	} 

	
	public ParserRule getEscapeRule()
	{
		return escapeRule;
	} 

	
	public void setEscapeRule(ParserRule escapeRule)
	{
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

	
	@Override
	public String toString()
	{
		return getClass().getName() + '[' + modeName + "::" + setName + ']';
	} 

	
	private static ParserRuleSet[] standard;

	static
	{
		standard = new ParserRuleSet[Token.ID_COUNT];
		for(byte i = 0; i < Token.ID_COUNT; i++)
		{
			standard[i] = new ParserRuleSet(null,null);
			standard[i].setDefault(i);
			standard[i].builtIn = true;
		}
	}

	private String modeName, setName;
	private Hashtable<String, String> props;

	private KeywordMap keywords;

	private int ruleCount;

	private Map<Character, List<ParserRule>> ruleMap;

	private final List<ParserRuleSet> imports;

	
	private int terminateChar = -1;
	private boolean ignoreCase = true;
	private byte defaultToken;
	private ParserRule escapeRule;

	private boolean highlightDigits;
	private Pattern digitRE;

	private String _noWordSep;
	private String noWordSep;

	private boolean builtIn;
	
}
