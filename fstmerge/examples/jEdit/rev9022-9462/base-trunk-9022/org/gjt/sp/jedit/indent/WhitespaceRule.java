

package org.gjt.sp.jedit.indent;

import org.gjt.sp.jedit.TextUtilities;
import org.gjt.sp.jedit.buffer.JEditBuffer;

import java.util.List;


public class WhitespaceRule implements IndentRule
{

	public void apply(JEditBuffer buffer, int thisLineIndex,
			  int prevLineIndex, int prevPrevLineIndex,
			  List<IndentAction> indentActions)
	{
		
		String current = buffer.getLineText(thisLineIndex);
		boolean found = false;
		for (int i = 0; i < current.length(); i++)
		{
			if (!Character.isWhitespace(current.charAt(i)))
			{
				found = true;
				break;
			}
		}
		if (!found)
			return;

		
		String previous = buffer.getLineText(prevLineIndex);
		for (int i = 0; i < previous.length(); i++)
		{
			if (!Character.isWhitespace(previous.charAt(i)))
				return;
		}
		indentActions.add(new IndentAction.NoIncrease());
	}

}

