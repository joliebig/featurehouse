

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

		CharSequence lineText = buffer.getLineSegment(index);
		int offset;
		for (offset = lineText.length() - 1; offset >= 0; offset--)
		{
			if (lineText.charAt(offset) == closeBracket)
				break;
		}
		if(offset == -1)
			return;

		int closeCount = getBrackets(buffer, index).closeCount;
		if(closeCount != 0)
		{
			AlignBracket alignBracket
				= new AlignBracket(buffer,index,offset);
			
			int openLine = alignBracket.getOpenBracketLine();
			if(openLine != -1)
			{
				int column = alignBracket.getOpenBracketColumn();
				alignBracket.setExtraIndent(
					getBrackets(buffer, openLine,
						0, column).openCount);
			}

			indentActions.add(alignBracket);
		}
	} 

	
	
	@Deprecated
	public boolean isMatch(String line)
	{
		return getBrackets(line).closeCount != 0;
	} 

	private boolean aligned;

	
	private static class AlignBracket implements IndentAction
	{
		private int line, offset;
		private int openBracketLine;
		private int openBracketColumn;
		private CharSequence openBracketLineText;
		private int extraIndent;

		public AlignBracket(JEditBuffer buffer, int line, int offset)
		{
			this.line = line;
			this.offset = offset;

			int openBracketIndex = TextUtilities.findMatchingBracket(
				buffer,this.line,this.offset);
			if(openBracketIndex == -1)
				openBracketLine = -1;
			else
			{
				openBracketLine = buffer.getLineOfOffset(openBracketIndex);
				openBracketColumn = openBracketIndex -
					buffer.getLineStartOffset(openBracketLine);
				openBracketLineText = buffer.getLineSegment(openBracketLine);
			}
		}

		public int getExtraIndent()
		{
			return extraIndent;
		}

		public void setExtraIndent(int extraIndent)
		{
			this.extraIndent = extraIndent;
		}

		public int getOpenBracketColumn()
		{
			return openBracketColumn;
		}

		public int getOpenBracketLine()
		{
			return openBracketLine;
		}

		public int calculateIndent(JEditBuffer buffer, int line, int oldIndent,
			int newIndent)
		{
			if(openBracketLineText == null)
				return newIndent;
			else
			{
				return StandardUtilities.getLeadingWhiteSpaceWidth(
					openBracketLineText,buffer.getTabSize())
					+ (extraIndent * buffer.getIndentSize());
			}
		}

		public boolean keepChecking()
		{
			return false;
		}
	} 
}
