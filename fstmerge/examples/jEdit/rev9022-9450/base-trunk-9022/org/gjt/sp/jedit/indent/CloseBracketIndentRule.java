

package org.gjt.sp.jedit.indent;

import java.util.List;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.TextUtilities;


public class CloseBracketIndentRule extends BracketIndentRule
{
	
	public CloseBracketIndentRule(char closeBracket, boolean aligned)
	{
		super(TextUtilities.getComplementaryBracket(closeBracket,
			new boolean[1]),closeBracket);
		this.aligned = aligned;
	} 

	
	public void apply(JEditBuffer buffer, int thisLineIndex,
		int prevLineIndex, int prevPrevLineIndex,
		List<IndentAction> indentActions)
	{
		int index;
		if(aligned)
			index = thisLineIndex;
		else
			index = prevLineIndex;

		if(index == -1)
			return;

		String line = buffer.getLineText(index);

		int offset = line.lastIndexOf(closeBracket);
		if(offset == -1)
			return;

		int closeCount = getBrackets(line).closeCount;
		if(closeCount != 0)
		{
			IndentAction.AlignBracket alignBracket
				= new IndentAction.AlignBracket(
				buffer,index,offset);
			
			String openLine = alignBracket.getOpenBracketLine();
			int column = alignBracket.getOpenBracketColumn();
			if(openLine != null)
			{
				String leadingBrackets = openLine.substring(0,column);
				alignBracket.setExtraIndent(getBrackets(leadingBrackets)
					.openCount);
			}

			indentActions.add(alignBracket);
		}
	} 

	
	public boolean isMatch(String line)
	{
		return getBrackets(line).closeCount != 0;
	} 

	private boolean aligned;
}
