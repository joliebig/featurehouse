

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
		this.regexp = Pattern.compile(regexp, Pattern.CASE_INSENSITIVE );
		this.collapse = collapse;
	} 

	
	public void apply(JEditBuffer buffer, int thisLineIndex,
		int prevLineIndex, int prevPrevLineIndex,
		List<IndentAction> indentActions)
	{
		if(thisAction != null
			&& isMatch(buffer.getLineText(thisLineIndex)))
		{
			indentActions.add(thisAction);
		}
		if(prevAction != null
			&& prevLineIndex != -1
			&& isMatch(buffer.getLineText(prevLineIndex)))
		{
			indentActions.add(prevAction);
			if (collapse)
				indentActions.add(IndentAction.PrevCollapse);
		}
		if(prevPrevAction != null
			&& prevPrevLineIndex != -1
			&& isMatch(buffer.getLineText(prevPrevLineIndex)))
		{
			indentActions.add(prevPrevAction);
			if (collapse)
				indentActions.add(IndentAction.PrevPrevCollapse);
		}
	} 

	
	public boolean isMatch(String line)
	{
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
