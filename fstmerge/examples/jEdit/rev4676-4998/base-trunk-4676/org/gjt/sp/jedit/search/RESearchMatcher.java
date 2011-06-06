

package org.gjt.sp.jedit.search;


import bsh.BshMethod;
import bsh.NameSpace;
import gnu.regexp.*;
import org.gjt.sp.jedit.BeanShell;
import org.gjt.sp.jedit.MiscUtilities;



public class RESearchMatcher implements SearchMatcher
{
	
	public static final RESyntax RE_SYNTAX_JEDIT
		= new RESyntax(RESyntax.RE_SYNTAX_PERL5)
		.set(RESyntax.RE_CHAR_CLASSES)
		.setLineSeparator("\n");

	
	
	public RESearchMatcher(String search, String replace,
		boolean ignoreCase, boolean beanshell,
		BshMethod replaceMethod) throws Exception
	{
		if(beanshell && replaceMethod != null && replace.length() != 0)
		{
			this.beanshell = true;
			this.replaceMethod = replaceMethod;
			replaceNS = new NameSpace(BeanShell.getNameSpace(),
				"search and replace");
		}
		else
		{
			
			
			this.replace = MiscUtilities.escapesToChars(replace);
		}

		re = new RE(search,(ignoreCase ? RE.REG_ICASE : 0)
			| RE.REG_MULTILINE,RE_SYNTAX_JEDIT);

		returnValue = new int[2];
	} 

	
	
	public int[] nextMatch(CharIndexed text, boolean start, boolean end,
		boolean firstTime, boolean reverse)
	{
		int flags = 0;

		
		
		if(!start)
			flags |= RE.REG_NOTBOL;
		
		
		if(!end)
			flags |= RE.REG_NOTEOL;

		REMatch match = re.getMatch(text,0,flags);
		if(match == null)
			return null;

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

		returnValue[0] = _start;
		returnValue[1] = _end;
		return returnValue;
	} 

	
	
	public String substitute(String text) throws Exception
	{
		REMatch match = re.getMatch(text);
		if(match == null)
			return null;

		if(beanshell)
		{
			int count = re.getNumSubs();
			for(int i = 0; i <= count; i++)
				replaceNS.setVariable("_" + i,match.toString(i));

			Object obj = BeanShell.runCachedBlock(replaceMethod,
				null,replaceNS);
			if(obj == null)
				return "";
			else
				return obj.toString();
		}
		else
			return match.substituteInto(replace);
	} 

	
	private String replace;
	private RE re;
	private boolean beanshell;
	private BshMethod replaceMethod;
	private NameSpace replaceNS;
	private int[] returnValue;
	
}
