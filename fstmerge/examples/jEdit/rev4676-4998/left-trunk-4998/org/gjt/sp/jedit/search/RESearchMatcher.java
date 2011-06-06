

package org.gjt.sp.jedit.search;


import bsh.BshMethod;
import bsh.NameSpace;
import gnu.regexp.*;
import org.gjt.sp.jedit.BeanShell;
import org.gjt.sp.jedit.MiscUtilities;



public class RESearchMatcher extends SearchMatcher
{
	
	public static final RESyntax RE_SYNTAX_JEDIT
		= new RESyntax(RESyntax.RE_SYNTAX_PERL5)
		.set(RESyntax.RE_CHAR_CLASSES)
		.setLineSeparator("\n");

	
	
	public RESearchMatcher(String search, boolean ignoreCase)
		throws REException
	{
		re = new RE(search,(ignoreCase ? RE.REG_ICASE : 0)
			| RE.REG_MULTILINE,RE_SYNTAX_JEDIT);
		returnValue = new Match();
	} 

	
	
	public SearchMatcher.Match nextMatch(CharIndexed text, boolean start,
		boolean end, boolean firstTime, boolean reverse)
	{
		int flags = 0;

		
		
		if(!start)
			flags |= RE.REG_NOTBOL;
		
		
		if(!end)
			flags |= RE.REG_NOTEOL;

		REMatch match = re.getMatch(text,0,flags);
		if(match == null)
			return null;

		returnValue.substitutions = new String[
			re.getNumSubs() + 1];
		for(int i = 0; i < returnValue.substitutions.length; i++)
		{
			returnValue.substitutions[i] = match.toString(i);
		}

		int _start = match.getStartIndex();
		int _end = match.getEndIndex();

		
		
		
		if(!firstTime && _start == 0 && _end == 0)
		{
			text.move(1);

			if(text.charAt(0) == CharIndexed.OUT_OF_BOUNDS)
			{
				
				return null;
			}

			match = re.getMatch(text,0,flags | RE.REG_NOTBOL);
			if(match == null)
				return null;
			else
			{
				_start = match.getStartIndex() + 1;
				_end = match.getEndIndex() + 1;
			}
		}

		returnValue.start = _start;
		returnValue.end = _end;
		return returnValue;
	} 

	
	private RE re;
	
}
