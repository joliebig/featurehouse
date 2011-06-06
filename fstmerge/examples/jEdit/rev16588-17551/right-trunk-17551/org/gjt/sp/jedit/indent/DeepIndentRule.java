

package org.gjt.sp.jedit.indent;

import org.gjt.sp.jedit.TextUtilities;
import org.gjt.sp.jedit.buffer.JEditBuffer;

import org.gjt.sp.jedit.syntax.Token;
import org.gjt.sp.jedit.syntax.TokenHandler;
import org.gjt.sp.jedit.syntax.TokenMarker;

import java.util.List;
import java.util.Stack;
import javax.swing.text.Segment;


public class DeepIndentRule implements IndentRule
{
	private final char openChar;
	private final char closeChar;

	public DeepIndentRule(char openChar, char closeChar)
	{
		this.openChar = openChar;
		this.closeChar = closeChar;
	}

	
	public void apply(JEditBuffer buffer, int thisLineIndex,
			  int prevLineIndex, int prevPrevLineIndex,
			  List<IndentAction> indentActions)
	{
		if (prevLineIndex == -1)
			return;

		int lineIndex = prevLineIndex;
		int oldLineIndex = lineIndex;
		CharSequence lineText = buffer.getLineSegment(lineIndex);
		int searchPos = -1;
		while (true)
		{
			if (lineIndex != oldLineIndex)
			{
				lineText = buffer.getLineSegment(lineIndex);
				oldLineIndex = lineIndex;
			}
			Parens parens = new Parens(buffer, lineIndex, searchPos);

			
			if (parens.openOffset == -1 && parens.closeOffset == -1)
			{
				
				if (prevPrevLineIndex != -1) {
					searchPos = -1;
					lineIndex = prevPrevLineIndex;
					prevPrevLineIndex = -1;
					continue;
				}
				return;
			}

			
			
			if (parens.closeOffset == -1)
			{
				
				int indent = parens.openOffset + getIndent(lineText, buffer.getTabSize()) - lineText.length();
				indentActions.clear();
				indentActions.add(new IndentAction.AlignParameter(indent));
				return;
			}

			
			
			int openParenOffset = TextUtilities.findMatchingBracket(buffer, lineIndex, parens.closeOffset);
			if (openParenOffset >= 0)
			{
				
				prevPrevLineIndex = -1;
				lineIndex = buffer.getLineOfOffset(openParenOffset);
				searchPos = openParenOffset - buffer.getLineStartOffset(lineIndex) - 1;
				if (searchPos < 0)
					break;
			}
			else
				break;
		}
	} 


	
	private int getIndent(CharSequence line, int tabSize)
	{
		int cnt = 0;
		for (int i = 0;  i < line.length(); i++)
		{
			if (line.charAt(i) == '\t')
			{
				cnt += tabSize;
			}
			else
			{
				if (!Character.isWhitespace(line.charAt(i)))
				{
					cnt += (line.length() - i);
					break;
				}
				cnt++;
			}
		}
		return cnt;
	}


	
	private class Parens implements TokenHandler
	{
		int openOffset;
		int closeOffset;

		private int searchPos;
		private Stack<Integer> open;
		private Stack<Integer> close;

		Parens(JEditBuffer b, int line, int pos)
		{
			this.searchPos = pos;
			this.open = new Stack<Integer>();
			this.close = new Stack<Integer>();
			b.markTokens(line, this);
			openOffset = (open.isEmpty()) ? -1 : open.pop();
			closeOffset = (close.isEmpty()) ? -1 : close.pop();
		}

		public void handleToken(Segment seg,
					byte id,
					int offset,
					int length,
					TokenMarker.LineContext context)
		{
			if (length <= 0 ||
			    (searchPos != -1 && searchPos < offset))
			{
				return;
			}

			if (searchPos != -1 && offset + length > searchPos)
			{
				length = searchPos - offset + 1;
			}

			switch (id)
			{
			case Token.COMMENT1:
			case Token.COMMENT2:
			case Token.COMMENT3:
			case Token.COMMENT4:
			case Token.LITERAL1:
			case Token.LITERAL2:
			case Token.LITERAL3:
			case Token.LITERAL4:
				
				break;
			default:
				for (int i = offset; i < offset + length; i++)
				{
					if (seg.array[seg.offset + i] == openChar)
					{
						if (open.isEmpty() && !close.isEmpty())
							close.pop();
						else
							open.push(i);
					}
					else if (seg.array[seg.offset + i] == closeChar)
					{
						if (close.isEmpty() && !open.isEmpty())
							open.pop();
						else
							close.push(i);
					}
				}
				break;
			}
		}

		public void setLineContext(TokenMarker.LineContext lineContext)
		{
			
		}

		@Override
		public String toString()
		{
			return "Parens(" + openOffset + ',' + closeOffset + ')';
		}
	} 

}

