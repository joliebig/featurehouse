

package org.gjt.sp.jedit.indent;

import org.gjt.sp.jedit.TextUtilities;
import org.gjt.sp.jedit.buffer.JEditBuffer;

import java.util.List;


public class DeepIndentRule implements IndentRule
{
	
	
	private static Parens getLastParens(String s, int pos)
	{
		int lastClose;
		int lastOpen;
		if (pos == -1)
		{
			lastClose = s.lastIndexOf(')');
			lastOpen = s.lastIndexOf('(');
		}
		else
		{
			lastClose = s.lastIndexOf(')', pos);
			lastOpen = s.lastIndexOf('(', pos);
		}
		return new Parens(lastOpen, lastClose);
	} 

	
	public void apply(JEditBuffer buffer, int thisLineIndex,
			  int prevLineIndex, int prevPrevLineIndex,
			  List<IndentAction> indentActions)
	{
		if (prevLineIndex == -1)
			return;
		
		int lineIndex = prevLineIndex;
		int oldLineIndex = lineIndex;
		String lineText = buffer.getLineText(lineIndex);
		int searchPos = -1;
		while (true)
		{
			if (lineIndex != oldLineIndex)
			{
				lineText = buffer.getLineText(lineIndex);
				oldLineIndex = lineIndex;
			}
			Parens parens = getLastParens(lineText, searchPos);
			if (parens.openOffset > parens.closeOffset)
			{
				
				int indent = parens.openOffset + TextUtilities.tabsToSpaces(lineText, buffer.getTabSize()).length() - lineText.length();
				indentActions.add(new IndentAction.AlignParameter(indent, lineText));
				return;
			}
			
			
			if (parens.openOffset == -1 && parens.closeOffset == -1)
			{
				return;
			}
			int openParenOffset = TextUtilities.findMatchingBracket(buffer, lineIndex, parens.closeOffset);
			if (openParenOffset >= 0)
			{
				lineIndex = buffer.getLineOfOffset(openParenOffset);
				searchPos = openParenOffset - buffer.getLineStartOffset(lineIndex) - 1;
				if (searchPos < 0)
				 	break;
			}
			else
				break;
		}
	} 

	
	private static class Parens
	{
		final int openOffset;
		final int closeOffset;
		
		Parens(int openOffset, int closeOffset)
		{
			this.openOffset = openOffset;
			this.closeOffset = closeOffset;
		}
		
		@Override
		public String toString()
		{
			return "Parens(" + openOffset + ',' + closeOffset + ')';
		}
	} 
}

