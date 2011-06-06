

package org.gjt.sp.jedit.search;

import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class PatternSearchMatcher extends SearchMatcher
{

	
	public PatternSearchMatcher(String search, boolean ignoreCase)
	{
		pattern = search;
		flags = Pattern.MULTILINE;
		if (ignoreCase)
			flags |= Pattern.CASE_INSENSITIVE;
	}

	
	public SearchMatcher.Match nextMatch(CharSequence text, boolean start,
		boolean end, boolean firstTime, boolean reverse)
	{
		if (re == null)
			re = Pattern.compile(pattern, flags);

		Matcher match = re.matcher(text);
		if (!match.find())
			return null;

		
		
		
		if (!start && match.start() == 0
			&& re.pattern().charAt(0) == '^' && !match.find())
			return null;

		
		
		
		if (!end && match.end() == (text.length() - 1)
			&& pattern.charAt(pattern.length() - 1) == '$')
			return null;

		returnValue.substitutions = new String[match.groupCount() + 1];
		for(int i = 0; i < returnValue.substitutions.length; i++)
		{
			returnValue.substitutions[i] = match.group(i);
		}

		int _start = match.start();
		int _end = match.end();

		returnValue.start = _start;
		returnValue.end = _end;
		return returnValue;
	}

	public boolean isMatchingEOL()
	{
		return pattern.charAt(pattern.length() - 1) == '$';
	}

	
	public String toString()
	{
		return "PatternSearchMatcher[" + pattern + ']';
	} 

        private int	flags;
	private Pattern	re;
	private String 	pattern;

}

