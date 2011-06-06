

package org.gjt.sp.jedit.indent;

import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.TextUtilities;
import org.gjt.sp.util.StandardUtilities;


public interface IndentAction
{
	
	int calculateIndent(JEditBuffer buffer, int line, int oldIndent,
		int newIndent);

	
	boolean keepChecking();

	
	class Collapse implements IndentAction
	{
		
		public int calculateIndent(JEditBuffer buffer, int line, int oldIndent,
			int newIndent)
		{
			return newIndent;
		}

		public boolean keepChecking()
		{
			return true;
		}

		private Collapse()
		{
		}
	}

	class Reset implements IndentAction
	{
		public int calculateIndent(JEditBuffer buffer, int line, int oldIndent,
			int newIndent)
		{
			return oldIndent;
		}

		public boolean keepChecking()
		{
			return true;
		}
	}

	class Increase implements IndentAction
	{
		private int amount;

		public Increase()
		{
			amount = 1;
		}

		public Increase(int amount)
		{
			this.amount = amount;
		}

		public int calculateIndent(JEditBuffer buffer, int line, int oldIndent,
			int newIndent)
		{
			return newIndent + buffer.getIndentSize() * amount;
		}

		public boolean keepChecking()
		{
			return true;
		}

		public boolean equals(Object o)
		{
			if(o instanceof Increase)
				return ((Increase)o).amount == amount;
			else
				return false;
		}
	}

	class Decrease implements IndentAction
	{
		public int calculateIndent(JEditBuffer buffer, int line, int oldIndent,
			int newIndent)
		{
			return newIndent - buffer.getIndentSize();
		}

		public boolean keepChecking()
		{
			return true;
		}
	}

	class AlignBracket implements IndentAction
	{
		private int line, offset;
		private int openBracketLine;
		private int openBracketColumn;
		private String openBracketLineText;
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
				openBracketLineText = buffer.getLineText(openBracketLine);
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

		public String getOpenBracketLine()
		{
			return openBracketLineText;
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

	
	class AlignOffset implements IndentAction
	{
		private int offset;

		public AlignOffset(int offset)
		{
			this.offset = offset;
		}

		public int calculateIndent(JEditBuffer buffer, int line, int oldIndent,
			int newIndent)
		{
			return offset;
		}

		public boolean keepChecking()
		{
			return false;
		}
	}

	
	class AlignParameter implements IndentAction
	{
		private int openParensColumn;
		private String openParensLineText;

		public AlignParameter(int openParensColumn, String openParensLineText)
		{
			this.openParensLineText = openParensLineText;
			this.openParensColumn = openParensColumn;
		}

		public int calculateIndent(JEditBuffer buffer, int line, int oldIndent,
				     int newIndent)
		{
			return openParensLineText == null ? newIndent : openParensColumn + 1;
		}

		public boolean keepChecking()
		{
			return false;
		}
	}

	
	class NoIncrease implements IndentAction
	{
		public int calculateIndent(JEditBuffer buffer, int line, int oldIndent,
				           int newIndent)
		{
			int current = StandardUtilities.getLeadingWhiteSpaceWidth(
					buffer.getLineText(line),buffer.getTabSize());
			return (current < newIndent) ? current : newIndent;
		}

		public boolean keepChecking()
		{
			return true;
		}
	}

	
	Collapse PrevCollapse		= new Collapse();
	
	Collapse PrevPrevCollapse	= new Collapse();
}

