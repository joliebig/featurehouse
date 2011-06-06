

package org.gjt.sp.jedit.indent;

import org.gjt.sp.jedit.TextUtilities;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.util.StandardUtilities;

import java.util.List;


public class DeepIndentRule implements IndentRule
{
	
	
	private static Parens getLastParens(CharSequence s, int pos)
	{
		int lastClose;
		int lastOpen;
		if (pos == -1)
		{
			lastClose = StandardUtilities.getLastIndexOf(s, ')');
			lastOpen = StandardUtilities.getLastIndexOf(s, '(');
		}
		else
		{
			lastClose = StandardUtilities.getLastIndexOf(s, ')', pos);
			lastOpen = StandardUtilities.getLastIndexOf(s, '(', pos);
		}
		return new Parens(lastOpen, lastClose);
	} 

	
	public void apply(IndentContext ctx)
	{
		int lineIndex = ctx.getLineIndex(-1);
		if (lineIndex == -1)
			return;

		int oldLineIndex = lineIndex;
		CharSequence lineText = ctx.getLineText(-1);

		int searchPos = -1;
		while (true)
		{
			if (lineIndex != oldLineIndex)
			{
				lineText = ctx.getBuffer().getLineText(lineIndex);
				oldLineIndex = lineIndex;
			}
			Parens parens = getLastParens(lineText, searchPos);
			if (parens.openOffset > parens.closeOffset)
			{
				
				int indent = parens.openOffset + TextUtilities.tabsToSpaces(lineText, ctx.getBuffer().getTabSize()).length() - lineText.length();
				ctx.addAction(new IndentAction.AlignParameter(indent, lineText));
				return;
			}

			
			if (parens.openOffset == -1 && parens.closeOffset == -1)
			{
				return;
			}
			int openParenOffset = TextUtilities.findMatchingBracket(ctx.getBuffer(), lineIndex, parens.closeOffset);
			if (openParenOffset >= 0)
			{
				lineIndex = ctx.getBuffer().getLineOfOffset(openParenOffset);
				searchPos = openParenOffset - ctx.getBuffer().getLineStartOffset(lineIndex) - 1;
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

		public String toString()
		{
			return "Parens(" + openOffset + ',' + closeOffset + ')';
		}
	} 
}

