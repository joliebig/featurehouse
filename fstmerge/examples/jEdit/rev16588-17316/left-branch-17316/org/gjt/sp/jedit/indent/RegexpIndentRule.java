

package org.gjt.sp.jedit.indent;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.swing.text.Segment;

import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.syntax.Token;
import org.gjt.sp.jedit.syntax.TokenHandler;
import org.gjt.sp.jedit.syntax.TokenMarker;


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
			&& lineMatches(buffer, thisLineIndex))
		{
			indentActions.add(thisAction);
		}
		if(prevAction != null
			&& prevLineIndex != -1
			&& lineMatches(buffer, prevLineIndex))
		{
			indentActions.add(prevAction);
			if (collapse)
				indentActions.add(IndentAction.PrevCollapse);
		}
		if(prevPrevAction != null
			&& prevPrevLineIndex != -1
			&& lineMatches(buffer, prevPrevLineIndex))
		{
			indentActions.add(prevPrevAction);
			if (collapse)
				indentActions.add(IndentAction.PrevPrevCollapse);
		}
	} 

	
	
	@Deprecated
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

	
	
	private static class TokenFilter implements TokenHandler
	{
		public StringBuilder result;

		public TokenFilter(int originalLength)
		{
			result = new StringBuilder(originalLength);
		}

		public void handleToken(Segment seg
			, byte id, int offset, int length
			, TokenMarker.LineContext context)
		{
			
			
			if (length <= 0)
			{
				return;
			}
			
			switch (id)
			{
			case Token.COMMENT1:
			case Token.COMMENT2:
			case Token.COMMENT3:
			case Token.COMMENT4:
				
				
				result.append(' ');
				break;
			case Token.LITERAL1:
			case Token.LITERAL2:
			case Token.LITERAL3:
			case Token.LITERAL4:
				
				
				
				result.append('0');
				break;
			default:
				result.append(seg.array
					, seg.offset + offset
					, length);
				break;
			}
		}

		public void setLineContext(TokenMarker.LineContext lineContext)
		{
		}
	} 

	
	private boolean lineMatches(JEditBuffer buffer, int lineIndex)
	{
		TokenFilter filter
			= new TokenFilter(buffer.getLineLength(lineIndex));
		buffer.markTokens(lineIndex, filter);
		return regexp.matcher(filter.result).matches();
	} 
}
