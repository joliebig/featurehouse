

package org.gjt.sp.jedit.indent;

import org.gjt.sp.jedit.TextUtilities;


public class WhitespaceRule implements IndentRule
{

	public void apply(IndentContext ctx)
	{
		
		CharSequence current = ctx.getLineText(0);
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

		
		CharSequence previous = ctx.getLineText(-1);
		for (int i = 0; i < previous.length(); i++)
		{
			if (!Character.isWhitespace(previous.charAt(i)))
				return;
		}
		ctx.addAction(new IndentAction.NoIncrease());
	}

}

