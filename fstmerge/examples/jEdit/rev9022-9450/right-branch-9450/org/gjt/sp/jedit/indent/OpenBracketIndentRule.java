

package org.gjt.sp.jedit.indent;

import java.util.List;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.TextUtilities;


public class OpenBracketIndentRule extends BracketIndentRule
{
	
	public OpenBracketIndentRule(char openBracket, boolean aligned)
	{
		super(openBracket,
			TextUtilities.getComplementaryBracket(openBracket,null));
		this.aligned = aligned;
	} 

	
	public void apply(IndentContext ctx)
	{
		int prevOpenBracketCount =
			getOpenBracketCount(ctx, -1);
		if(prevOpenBracketCount != 0)
		{
			handleCollapse(ctx.getActions(), true);
			boolean multiple = ctx.getBuffer().getBooleanProperty(
				"multipleBracketIndent");
			IndentAction increase = new IndentAction.Increase(
				multiple ? prevOpenBracketCount : 1);
			ctx.addAction(increase);
		}
		else if(getOpenBracketCount(ctx, 0) != 0)
		{
			handleCollapse(ctx.getActions(), false);
		}
	} 

	
	private int getOpenBracketCount(IndentContext ctx, int offset)
	{
		CharSequence line = ctx.getLineText(offset);
		return (line != null) ? getBrackets(ctx, line).openCount : 0;
	} 

	
	private static void handleCollapse(List<IndentAction> indentActions,
					   boolean delPrevPrevCollapse)
	{
		if (indentActions == null)
			return;
		if (indentActions.contains(IndentAction.PrevCollapse))
		{
			indentActions.clear();
			return;
		}

		if (delPrevPrevCollapse && indentActions.contains(IndentAction.PrevPrevCollapse))
		{
			indentActions.clear();
			return;
		}
	} 

	private boolean aligned;
}

