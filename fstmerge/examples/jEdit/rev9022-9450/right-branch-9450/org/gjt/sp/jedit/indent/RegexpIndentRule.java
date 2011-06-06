

package org.gjt.sp.jedit.indent;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.gjt.sp.jedit.buffer.JEditBuffer;


public class RegexpIndentRule implements IndentRule
{
	
	
	public RegexpIndentRule(String regexp, IndentAction prevPrev,
		IndentAction prev, IndentAction thisLine, boolean collapse)
	throws PatternSyntaxException
	{
		prevPrevAction = prevPrev;
		prevAction = prev;
		thisAction = thisLine;
		this.regexp = Pattern.compile(regexp, Pattern.CASE_INSENSITIVE);
		this.collapse = collapse;
	} 

	
	public void apply(IndentContext ctx)
	{
		if(thisAction != null
			&& isMatch(ctx.getLineText(0)))
		{
			ctx.addAction(thisAction);
		}
		if(prevAction != null
			&& isMatch(ctx.getLineText(-1)))
		{
			ctx.addAction(prevAction);
			if (collapse)
				ctx.addAction(IndentAction.PrevCollapse);
		}
		if(prevPrevAction != null
			&& isMatch(ctx.getLineText(-2)))
		{
			ctx.addAction(prevPrevAction);
			if (collapse)
				ctx.addAction(IndentAction.PrevPrevCollapse);
		}
	} 

	
	public boolean isMatch(CharSequence line)
	{
		if (line == null)
			return false;
		Matcher m = regexp.matcher(line);
		return m.matches();
	} 

	
	public String toString()
	{
		return getClass().getName() + '[' + regexp + ']';
	} 

	private IndentAction prevPrevAction, prevAction, thisAction;
	private Pattern regexp;
	private boolean collapse;
}
