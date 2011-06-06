

package org.gjt.sp.jedit.indent;

import java.util.List;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.TextUtilities;
import org.gjt.sp.util.StandardUtilities;


public class CloseBracketIndentRule extends BracketIndentRule
{
	
	public CloseBracketIndentRule(char closeBracket, boolean aligned)
	{
		super(TextUtilities.getComplementaryBracket(closeBracket,null),
			closeBracket);
		this.aligned = aligned;
	} 

	
	public void apply(IndentContext ctx)
	{
		int lOffset = aligned ? 0 : -1;
		CharSequence line = ctx.getLineText(lOffset);
		if (line == null)
			return;

		int offset = StandardUtilities.getLastIndexOf(line, closeBracket);
		if(offset == -1)
			return;

		int closeCount = getBrackets(ctx, lOffset).closeCount;
		if(closeCount != 0)
		{
			IndentAction.AlignBracket alignBracket
				= new IndentAction.AlignBracket(
				ctx.getBuffer(),ctx.getLineIndex(lOffset),offset);
			
			CharSequence openLine = alignBracket.getOpenBracketLine();
			int column = alignBracket.getOpenBracketColumn();
			if(openLine != null)
			{
				CharSequence leadingBrackets = openLine.subSequence(0,column);
				alignBracket.setExtraIndent(
					getBrackets(ctx, leadingBrackets).openCount);
			}

			ctx.addAction(alignBracket);
		}
	} 

	private boolean aligned;
}

