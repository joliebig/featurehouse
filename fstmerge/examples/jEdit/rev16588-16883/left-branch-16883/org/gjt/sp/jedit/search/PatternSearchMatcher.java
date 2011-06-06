

package org.gjt.sp.jedit.search;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.gjt.sp.util.ReverseCharSequence;


public class PatternSearchMatcher extends SearchMatcher
{
	
	
	public PatternSearchMatcher(String search, boolean ignoreCase)
	{
		pattern = search;
		flags = getFlag(ignoreCase);
	}
	
	
	public PatternSearchMatcher(Pattern re, boolean ignoreCase)
	{
		this(re.pattern(), ignoreCase);
		this.re = re;
	} 

	
	
	@Override
	public SearchMatcher.Match nextMatch(CharSequence text, boolean start,
		boolean end, boolean firstTime, boolean reverse)
	{
		
		
		
		
		
		
		
		
		
		
		if (text instanceof ReverseCharSequence)
			text = ((ReverseCharSequence)text).baseSequence();

		if (re == null)
			re = Pattern.compile(pattern, flags);

		Matcher match = re.matcher(text);
		if (!match.find())
			return null;

		
		
		
		if (!start && match.start() == 0
			&& re.pattern().charAt(0) == '^' && !match.find())
			return null;

		
		
		
		if (!reverse && !firstTime && match.start() == 0 && match.end() == 0)
		{
			if (!match.find())
				return null;
		}

		Match previous = null;
		while (true)
		{
			
			
			
			
			
			
			if ((!end || (text.charAt(text.length() - 1) == '\n'))
				&& match.end() == text.length()
				&& pattern.charAt(pattern.length() - 1) == '$')
			{
				if (previous != null)
				{
					returnValue.start = previous.start;
					returnValue.end = previous.end;
					returnValue.substitutions = previous.substitutions;
					break;
				}
				else
				{
					return null;
				}
			}

			returnValue.substitutions = new String[match.groupCount() + 1];
			for(int i = 0; i < returnValue.substitutions.length; i++)
			{
				returnValue.substitutions[i] = match.group(i);
			}
	
			int _start = match.start();
			int _end = match.end();
	
			returnValue.start = _start;
			returnValue.end = _end;
			
			
			
			
			if (!reverse || !match.find())
			{
				
				
				if (reverse && !firstTime && returnValue.start == text.length()
					&& returnValue.end == text.length())
				{
					if (previous != null)
					{
						returnValue.start = previous.start;
						returnValue.end = previous.end;
						returnValue.substitutions = previous.substitutions;
					}
					else
					{
						return null;
					}
				}
				break;
			}
			
			if (previous == null)
			{
				previous = new Match();
			}
			previous.start = returnValue.start;
			previous.end = returnValue.end;
			previous.substitutions = returnValue.substitutions;
		}

		if (reverse)
		{
			
			
			
			int len = returnValue.end - returnValue.start;
			returnValue.start = text.length() - returnValue.end;
			returnValue.end = returnValue.start + len;
		}

		return returnValue;
	} 

	
	@Override
	public String toString()
	{
		boolean ignoreCase = (flags & Pattern.CASE_INSENSITIVE) != 0;
		return "PatternSearchMatcher[" + pattern + ',' + ignoreCase + ']';
	} 
	
	static int getFlag(boolean ignoreCase)
	{
		int flags = Pattern.MULTILINE;
		if (ignoreCase)
			flags |= Pattern.CASE_INSENSITIVE;
		return flags;
	}

	
	private int flags;
	private Pattern	re;
	private final String pattern;
	
}

