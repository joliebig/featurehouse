

package org.gjt.sp.jedit.indent;

import java.util.List;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.TextUtilities;


public class OpenBracketIndentRule extends BracketIndentRule
{
	
	public OpenBracketIndentRule(char openBracket, boolean aligned)
	{
		super(openBracket,
			TextUtilities.getComplementaryBracket(openBracket,
			new boolean[1]));
		this.aligned = aligned;
	} 

	
	public void apply(JEditBuffer buffer, int thisLineIndex,
		int prevLineIndex, int prevPrevLineIndex,
		List<IndentAction> indentActions)
	{
		int prevOpenBracketCount = getOpenBracketCount(buffer,prevLineIndex);
		if(prevOpenBracketCount != 0)
		{
			handleCollapse(indentActions, true);
			boolean multiple = buffer.getBooleanProperty(
				"multipleBracketIndent");
			IndentAction increase = new IndentAction.Increase(
				multiple ? prevOpenBracketCount : 1);
			indentActions.add(increase);
		}
		else if(getOpenBracketCount(buffer,thisLineIndex) != 0)
		{
			handleCollapse(indentActions, false);
		}
	} 

	
	private int getOpenBracketCount(JEditBuffer buffer, int line)
	{
		if(line == -1)
			return 0;
		else
			return getBrackets(buffer.getLineText(line)).openCount;
	} 

	
	private static void handleCollapse(List<IndentAction> indentActions,
					   boolean delPrevPrevCollapse)
	{
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
