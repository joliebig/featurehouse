

package org.gjt.sp.jedit.search;


import bsh.BshMethod;
import bsh.NameSpace;
import gnu.regexp.CharIndexed;
import org.gjt.sp.jedit.BeanShell;



public class BoyerMooreSearchMatcher extends SearchMatcher
{
	
	
	public BoyerMooreSearchMatcher(String pattern, boolean ignoreCase)
	{
		this.pattern = pattern.toCharArray();
		if(ignoreCase)
		{
			for(int i = 0; i < this.pattern.length; i++)
			{
				this.pattern[i] = Character.toUpperCase(
					this.pattern[i]);
			}
		}

		this.replace = replace;
		this.ignoreCase = ignoreCase;

		pattern_end = this.pattern.length - 1;
	} 

	
	
	public SearchMatcher.Match nextMatch(CharIndexed text,
		boolean start, boolean end, boolean firstTime,
		boolean reverse)
	{
		int pos = match(text,reverse);

		if (pos == -1)
		{
			return null;
		}
		else
		{
			returnValue.start = pos;
			returnValue.end = pos + pattern.length;
			return returnValue;
		}
	} 

	
	
	public int match(CharIndexed text, boolean reverse)
	{
		
		
		
		int[] skip, suffix;
		if(reverse)
		{
			if(back_skip == null)
			{
				back_skip = generateSkipArray(true);
				back_suffix = generateSuffixArray(true);
			}
			skip = back_skip;
			suffix = back_suffix;
		}
		else
		{
			if(fwd_skip == null)
			{
				fwd_skip = generateSkipArray(false);
				fwd_suffix = generateSuffixArray(false);
			}
			skip = fwd_skip;
			suffix = fwd_suffix;
		} 

		
		int pos;

		
		int anchor = 0;

		
		
		
		
		
		

		char ch = 0;

		int bad_char;
		int good_suffix;

		
		
		
		
		
		
		
		
		
		
SEARCH:
		while (text.isValid())
		{
			for (pos = pattern_end; pos >= 0; --pos)
			{
				ch = text.charAt(pos);
				if(ignoreCase)
					ch = Character.toUpperCase(ch);

				
				if ((reverse ? ch != pattern[pattern_end - pos]
					: ch != pattern[pos]))
				{
					

					
					bad_char = pos - skip[getSkipIndex(ch)];

					
					good_suffix = suffix[pos];

					
					
					int skip_index = (bad_char > good_suffix) ? bad_char : good_suffix;
					anchor += skip_index;
					text.move(skip_index);

					
					continue SEARCH;
				}
			}

			
			return anchor;
		}

		
		return -1;
	} 

	
	private char[] pattern;
	private int pattern_end;
	private String replace;
	private boolean ignoreCase;

	
	private int[] fwd_skip;
	private int[] fwd_suffix;
	private int[] back_skip;
	private int[] back_suffix;
	

	

	
	
	private int[] generateSkipArray(boolean reverse)
	{
		
		int[] skip = new int[256];

		
		if (pattern.length == 0)
			return skip;

		int pos = 0;

		do
		{
			skip[getSkipIndex(pattern[reverse ? pattern_end - pos : pos])] = pos;
		}
		while (++pos < pattern.length);

		return skip;
	} 

	
	
	private static final int getSkipIndex(char ch)
	{
		return ((int) ch) & 0x000000FF;
	} 

	
	
	private int[] generateSuffixArray(boolean reverse)
	{
		int m = pattern.length;

		int j = m + 1;

		int[] suffix = new int[j];
		int[] tmp = new int[j];
		tmp[m] = j;

		for (int i = m; i > 0; --i)
		{
			while (j <= m && pattern[reverse ? pattern_end - i + 1 : i - 1]
				!= pattern[reverse ? pattern_end - j + 1 : j - 1])
			{
				if (suffix[j] == 0)
				{
					suffix[j] = j - i;
				}

				j = tmp[j];
			}

			tmp[i - 1] = --j;
		}

		int k = tmp[0];

		for (j = 0; j <= m; j++)
		{
			
			
			
			if (j > 0)
			{
				suffix[j - 1] = (suffix[j] == 0) ? k : suffix[j];
			}

			if (j == k)
			{
				k = tmp[k];
			}
		}

		return suffix;
	} 

	
}
