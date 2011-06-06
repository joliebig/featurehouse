

package org.gjt.sp.jedit;


import java.lang.reflect.Method;
import java.util.Hashtable;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import org.gjt.sp.jedit.indent.DeepIndentRule;
import org.gjt.sp.jedit.indent.IndentRule;
import org.gjt.sp.jedit.indent.IndentRuleFactory;
import org.gjt.sp.jedit.indent.WhitespaceRule;
import org.gjt.sp.jedit.syntax.TokenMarker;
import org.gjt.sp.jedit.syntax.ModeProvider;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.StandardUtilities;



public class Mode
{
	
	
	public Mode(String name)
	{
		this.name = name;
		this.ignoreWhitespace = true;
		props = new Hashtable<String, Object>();
	} 

	
	
	public void init()
	{
		try
		{
			String filenameGlob = (String)getProperty("filenameGlob");
			if(filenameGlob != null && filenameGlob.length() != 0)
			{
				filenameRE = Pattern.compile(StandardUtilities.globToRE(filenameGlob),
							     Pattern.CASE_INSENSITIVE);
			}

			String firstlineGlob = (String)getProperty("firstlineGlob");
			if(firstlineGlob != null && firstlineGlob.length() != 0)
			{
				firstlineRE = Pattern.compile(StandardUtilities.globToRE(firstlineGlob),
							      Pattern.CASE_INSENSITIVE);
			}
		}
		catch(PatternSyntaxException re)
		{
			Log.log(Log.ERROR,this,"Invalid filename/firstline"
				+ " globs in mode " + name);
			Log.log(Log.ERROR,this,re);
		}

		
		
		
		
		
		
		marker = null;
	} 

	
	
	public TokenMarker getTokenMarker()
	{
		loadIfNecessary();
		return marker;
	} 

	
	
	public void setTokenMarker(TokenMarker marker)
	{
		this.marker = marker;
	} 

	
	
	public void loadIfNecessary()
	{
		if(marker == null)
		{
			ModeProvider.instance.loadMode(this);
			if (marker == null)
				Log.log(Log.ERROR, this, "Mode not correctly loaded, token marker is still null");
		}
	} 

	
	
	public Object getProperty(String key)
	{
		Object value = props.get(key);
		if(value != null)
			return value;
		return null;
	} 

	
	
	public boolean getBooleanProperty(String key)
	{
		Object value = getProperty(key);
		return StandardUtilities.getBoolean(value, false);
	} 

	
	
	public void setProperty(String key, Object value)
	{
		props.put(key,value);
	} 

	
	
	public void unsetProperty(String key)
	{
		props.remove(key);
	} 

	
	
	public void setProperties(Map props)
	{
		if(props == null)
			props = new Hashtable<String, Object>();

		ignoreWhitespace = !"false".equalsIgnoreCase(
					(String)props.get("ignoreWhitespace"));

		
		
		
		String filenameGlob = (String)this.props.get("filenameGlob");
		String firstlineGlob = (String)this.props.get("firstlineGlob");
		String filename = (String)this.props.get("file");
		this.props = props;
		if(filenameGlob != null)
			props.put("filenameGlob",filenameGlob);
		if(firstlineGlob != null)
			props.put("firstlineGlob",firstlineGlob);
		if(filename != null)
			props.put("file",filename);
	} 

	
	
	public boolean accept(String fileName, String firstLine)
	{
		return acceptFilename(fileName) || acceptFirstLine(firstLine);
	} 

	
	
	public boolean acceptFilename(String fileName)
	{
		return filenameRE != null && filenameRE.matcher(fileName).matches();
	} 

	
	
	public boolean acceptFirstLine(String firstLine)
	{
		return firstlineRE != null && firstlineRE.matcher(firstLine).matches();
	} 

	
	
	public String getName()
	{
		return name;
	} 

	
	
	public String toString()
	{
		return name;
	} 
	
	
	
	public boolean equals(Object other)
	{
		if (!(other instanceof Mode))
		{
			return false;	
		}
		String otherName = ((Mode)other).getName();
		return name.equals(otherName);
	} 
	
	
	
	public int hashCode()
	{
		return name.hashCode();
	} 
	
	
	public boolean getIgnoreWhitespace()
	{
		return ignoreWhitespace;
	} 

	

	public synchronized List<IndentRule> getIndentRules()
	{
		if (indentRules == null)
		{
			initIndentRules();
		}
		return indentRules;
	}

	public synchronized boolean isElectricKey(char ch)
	{
		if (electricKeys == null)
		{
			String[] props = {
				"indentOpenBrackets",
				"indentCloseBrackets",
				"electricKeys"
			};

			StringBuilder buf = new StringBuilder();
			for(int i = 0; i < props.length; i++)
			{
				String prop = (String) getProperty(props[i]);
				if (prop != null)
					buf.append(prop);
			}

			electricKeys = buf.toString();
		}

		return (electricKeys.indexOf(ch) >= 0);
	}

	private void initIndentRules()
	{
		List<IndentRule> rules = new LinkedList<IndentRule>();

		String[] regexpProps = {
			"indentNextLine",
			"indentNextLines"
		};

		for(int i = 0; i < regexpProps.length; i++)
		{
			IndentRule rule = createRegexpIndentRule(regexpProps[i]);
			if(rule != null)
				rules.add(rule);
		}

		String[] bracketProps = {
			"indentOpenBracket",
			"indentCloseBracket",
			"unalignedOpenBracket",
			"unalignedCloseBracket",
		};

		for(int i = 0; i < bracketProps.length; i++)
		{
			createBracketIndentRules(bracketProps[i], rules);
		}

		String[] finalProps = {
			"unindentThisLine",
			"unindentNextLines"
		};

		for(int i = 0; i < finalProps.length; i++)
		{
			IndentRule rule = createRegexpIndentRule(finalProps[i]);
			if(rule != null)
				rules.add(rule);
		}

		if (getBooleanProperty("deepIndent"))
		{
			String unalignedOpenBrackets = (String) getProperty("unalignedOpenBrackets");
			if (unalignedOpenBrackets != null)
			{
				for (int i = 0 ; i < unalignedOpenBrackets.length();i++)
				{
					char openChar = unalignedOpenBrackets.charAt(i);
					char closeChar = TextUtilities.getComplementaryBracket(openChar, null);
					if (closeChar != '\0')
						rules.add(new DeepIndentRule(openChar, closeChar));
				}
			}
		}

		if (!getIgnoreWhitespace())
			rules.add(new WhitespaceRule());

		indentRules = Collections.unmodifiableList(rules);
	}

	private IndentRule createRegexpIndentRule(String prop)
	{
		String value = (String) getProperty(prop);

		try
		{
			if(value != null)
			{
				Method m = IndentRuleFactory.class.getMethod(
					prop,new Class[] { String.class });
				return (IndentRule)m.invoke(null, value);
			}
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,this,"Bad indent rule " + prop
				+ '=' + value + ':');
			Log.log(Log.ERROR,this,e);
		}

		return null;
	}

	private void createBracketIndentRules(String prop,
						List<IndentRule> rules)
	{
		String value = (String) getProperty(prop + 's');

		try
		{
			if(value != null)
			{
				for(int i = 0; i < value.length(); i++)
				{
					char ch = value.charAt(i);

					Method m = IndentRuleFactory.class.getMethod(
						prop,new Class[] { char.class });
					rules.add((IndentRule) m.invoke(null, ch));
				}
			}
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,this,"Bad indent rule " + prop
				+ '=' + value + ':');
			Log.log(Log.ERROR,this,e);
		}
	}

	

	
	protected String name;
	protected Map<String, Object> props;
	private Pattern firstlineRE;
	private Pattern filenameRE;
	protected TokenMarker marker;
	private List<IndentRule> indentRules;
	private String electricKeys;
	private boolean ignoreWhitespace;
	
}
