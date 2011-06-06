

package org.gjt.sp.jedit.indent;

import javax.swing.text.Segment;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.syntax.Token;
import org.gjt.sp.jedit.syntax.TokenHandler;
import org.gjt.sp.jedit.syntax.TokenMarker;


public abstract class BracketIndentRule implements IndentRule
{
	
	public BracketIndentRule(char openBracket, char closeBracket)
	{
		this.openBracket = openBracket;
		this.closeBracket = closeBracket;
	} 

	
	public static class Brackets
	{
		int openCount;
		int closeCount;
	} 

	
	
	@Deprecated
	public Brackets getBrackets(String line)
	{
		Brackets brackets = new Brackets();

		for(int i = 0; i < line.length(); i++)
		{
			char ch = line.charAt(i);
			if(ch == openBracket)
			{
				
				if(line.length() - i >= 3)
				{
					if(line.substring(i,i+3).equals("{{{")) 
					{
						i += 2;
						continue;
					}
				}
				brackets.openCount++;
			}
			else if(ch == closeBracket)
			{
				if(brackets.openCount != 0)
					brackets.openCount--;
				else
					brackets.closeCount++;
			}
		}

		return brackets;
	} 

	
	public Brackets getBrackets(JEditBuffer buffer, int lineIndex)
	{
		return getBrackets(buffer, lineIndex,
			0, buffer.getLineLength(lineIndex));
	} 

	
	public Brackets getBrackets(JEditBuffer buffer, int lineIndex,
		int begin, int end)
	{
		LineScanner scanner = new LineScanner(begin, end);
		buffer.markTokens(lineIndex, scanner);
		return scanner.result;
	} 

	
	public String toString()
	{
		return getClass().getName() + "[" + openBracket + ","
			+ closeBracket + "]";
	} 

	protected char openBracket, closeBracket;

	
	private class LineScanner implements TokenHandler
	{
		public final Brackets result;

		private int scannedIndex;
		private final int beginIndex;
		private final int endIndex;

		public LineScanner(int begin, int end)
		{
			this.result = new Brackets();
			this.scannedIndex = 0;
			this.beginIndex = begin;
			this.endIndex = end;
		}

		boolean rejectsToken(byte id)
		{
			
			
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
				return true;
			default:
				return false;
			}
		}

		private void scan(Segment seg, int offset, int length)
		{
			int index = scannedIndex;
			if (index >= endIndex)
			{
				return;
			}
			if (index < beginIndex)
			{
				int numToSkip = beginIndex - index;
				if (numToSkip >= length)
				{
					return;
				}
				offset += numToSkip;
				length -= numToSkip;
				index = beginIndex;
			}
			if (index + length > endIndex)
			{
				length = endIndex - index;
			}

			for (int i = 0; i < length; ++i)
			{
				char c = seg.array[seg.offset + offset + i];
				if(c == openBracket)
				{
					result.openCount++;
				}
				else if(c == closeBracket)
				{
					if(result.openCount != 0)
						result.openCount--;
					else
						result.closeCount++;
				}
			}
		}

		public void handleToken(Segment seg
			, byte id, int offset, int length
			, TokenMarker.LineContext context)
		{
			if (!rejectsToken(id))
			{
				scan(seg, offset, length);
			}
			scannedIndex += length;
		}

		public void setLineContext(TokenMarker.LineContext lineContext)
		{
		}
	} 
}
