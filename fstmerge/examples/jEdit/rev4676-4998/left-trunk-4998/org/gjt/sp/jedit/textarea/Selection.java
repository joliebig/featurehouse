

package org.gjt.sp.jedit.textarea;


import java.util.ArrayList;
import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.MiscUtilities;



public abstract class Selection implements Cloneable
{
	
	
	public int getStart()
	{
		return start;
	} 

	
	
	public int getEnd()
	{
		return end;
	} 

	
	
	public abstract int getStart(Buffer buffer, int line);
	

	
	
	public abstract int getEnd(Buffer buffer, int line);
	

	
	
	public int getStartLine()
	{
		return startLine;
	} 

	
	
	public int getEndLine()
	{
		return endLine;
	} 

	
	
	public boolean overlaps(Selection s)
	{
		if((start >= s.start && start <= s.end)
			|| (end >= s.start && end <= s.end))
			return true;
		else
			return false;
	} 

	
	public String toString()
	{
		return getClass().getName() + "[start=" + start
			+ ",end=" + end + ",startLine=" + startLine
			+ ",endLine=" + endLine + "]";
	} 

	
	public Object clone()
	{
		try
		{
			return super.clone();
		}
		catch(CloneNotSupportedException e)
		{
			throw new InternalError("I just drank a whole "
				+ "bottle of cough syrup and I feel "
				+ "funny!");
		}
	} 

	
	int start, end;
	int startLine, endLine;

	
	Selection()
	{
	} 

	
	Selection(Selection sel)
	{
		this.start = sel.start;
		this.end = sel.end;
	} 

	
	Selection(int start, int end)
	{
		this.start = start;
		this.end = end;
	} 

	
	abstract void getText(Buffer buffer, StringBuffer buf);
	abstract int setText(Buffer buffer, String text);

	abstract boolean contentInserted(Buffer buffer, int startLine, int start,
		int numLines, int length);
	abstract boolean contentRemoved(Buffer buffer, int startLine, int start,
		int numLines, int length);
	

	
	
	public static class Range extends Selection
	{
		
		public Range()
		{
		} 

		
		public Range(Selection sel)
		{
			super(sel);
		} 

		
		public Range(int start, int end)
		{
			super(start,end);
		} 

		
		public int getStart(Buffer buffer, int line)
		{
			if(line == startLine)
				return start;
			else
				return buffer.getLineStartOffset(line);
		} 

		
		public int getEnd(Buffer buffer, int line)
		{
			if(line == endLine)
				return end;
			else
				return buffer.getLineEndOffset(line) - 1;
		} 

		

		
		void getText(Buffer buffer, StringBuffer buf)
		{
			buf.append(buffer.getText(start,end - start));
		} 

		
		int setText(Buffer buffer, String text)
		{
			buffer.remove(start,end - start);
			if(text != null && text.length() != 0)
			{
				buffer.insert(start,text);
				return start + text.length();
			}
			else
				return start;
		} 

		
		boolean contentInserted(Buffer buffer, int startLine, int start,
			int numLines, int length)
		{
			boolean changed = false;

			if(this.start >= start)
			{
				this.start += length;
				if(numLines != 0)
					this.startLine = buffer.getLineOfOffset(this.start);
				changed = true;
			}

			if(this.end >= start)
			{
				this.end += length;
				if(numLines != 0)
					this.endLine = buffer.getLineOfOffset(this.end);
				changed = true;
			}

			return changed;
		} 

		
		boolean contentRemoved(Buffer buffer, int startLine, int start,
			int numLines, int length)
		{
			int end = start + length;
			boolean changed = false;

			if(this.start > start && this.start <= end)
			{
				this.start = start;
				changed = true;
			}
			else if(this.start > end)
			{
				this.start -= length;
				changed = true;
			}

			if(this.end > start && this.end <= end)
			{
				this.end = start;
				changed = true;
			}
			else if(this.end > end)
			{
				this.end -= length;
				changed = true;
			}

			if(changed && numLines != 0)
			{
				this.startLine = buffer.getLineOfOffset(this.start);
				this.endLine = buffer.getLineOfOffset(this.end);
			}

			return changed;
		} 

		
	} 

	
	
	
	public static class Rect extends Selection
	{
		
		public Rect()
		{
			super();
		} 

		
		public Rect(Selection sel)
		{
			super(sel);
		} 

		
		public Rect(int start, int end)
		{
			super(start,end);
		} 

		
		public Rect(int startLine, int start, int endLine, int end)
		{
			this.startLine = startLine;
			this.start = start;
			this.endLine = endLine;
			this.end = end;
		} 

		
		public Rect(Buffer buffer, int startLine, int startColumn,
			int endLine, int endColumn)
		{
			this.startLine = startLine;
			this.endLine = endLine;

			int[] width = new int[1];
			int startOffset = buffer.getOffsetOfVirtualColumn(startLine,
				startColumn,width);
			if(startOffset == -1)
			{
				extraStartVirt = startColumn - width[0];
				startOffset = buffer.getLineEndOffset(startLine) - 1;
			}
			else
				startOffset += buffer.getLineStartOffset(startLine);

			int endOffset = buffer.getOffsetOfVirtualColumn(endLine,
				endColumn,width);
			if(endOffset == -1)
			{
				extraEndVirt = endColumn - width[0];
				endOffset = buffer.getLineEndOffset(endLine) - 1;
			}
			else
				endOffset += buffer.getLineStartOffset(endLine);
		} 

		
		public int getStartColumn(Buffer buffer)
		{
			int virtColStart = buffer.getVirtualWidth(startLine,
				start - buffer.getLineStartOffset(startLine)) + extraStartVirt;
			int virtColEnd = buffer.getVirtualWidth(endLine,
				end - buffer.getLineStartOffset(endLine)) + extraEndVirt;
			return Math.min(virtColStart,virtColEnd);
		} 

		
		public int getEndColumn(Buffer buffer)
		{
			int virtColStart = buffer.getVirtualWidth(startLine,
				start - buffer.getLineStartOffset(startLine)) + extraStartVirt;
			int virtColEnd = buffer.getVirtualWidth(endLine,
				end - buffer.getLineStartOffset(endLine)) + extraEndVirt;
			return Math.max(virtColStart,virtColEnd);
		} 

		
		public int getStart(Buffer buffer, int line)
		{
			return getColumnOnOtherLine(buffer,line,
				getStartColumn(buffer));
		} 

		
		public int getEnd(Buffer buffer, int line)
		{
			return getColumnOnOtherLine(buffer,line,
				getEndColumn(buffer));
		} 

		
		int extraStartVirt;
		int extraEndVirt;

		
		void getText(Buffer buffer, StringBuffer buf)
		{
			int start = getStartColumn(buffer);
			int end = getEndColumn(buffer);

			for(int i = startLine; i <= endLine; i++)
			{
				int lineStart = buffer.getLineStartOffset(i);
				int lineLen = buffer.getLineLength(i);

				int rectStart = buffer.getOffsetOfVirtualColumn(
					i,start,null);
				if(rectStart == -1)
					rectStart = lineLen;

				int rectEnd = buffer.getOffsetOfVirtualColumn(
					i,end,null);
				if(rectEnd == -1)
					rectEnd = lineLen;

				if(rectEnd < rectStart)
					System.err.println(i + ":::" + start + ":" + end
						+ " ==> " + rectStart + ":" + rectEnd);
				buf.append(buffer.getText(lineStart + rectStart,
					rectEnd - rectStart));

				if(i != endLine)
					buf.append('\n');
			}
		} 

		
		int setText(Buffer buffer, String text)
		{
			int startColumn = getStartColumn(buffer);
			int endColumn = getEndColumn(buffer);

			int[] total = new int[1];

			int tabSize = buffer.getTabSize();

			int maxWidth = 0;
			int totalLines = 0;
			ArrayList lines = new ArrayList();

			
			if(text != null)
			{
				int lastNewline = 0;
				int currentWidth = startColumn;
				for(int i = 0; i < text.length(); i++)
				{
					char ch = text.charAt(i);
					if(ch == '\n')
					{
						totalLines++;
						lines.add(text.substring(
							lastNewline,i));
						lastNewline = i + 1;
						maxWidth = Math.max(maxWidth,currentWidth);
						lines.add(new Integer(currentWidth));
						currentWidth = startColumn;
					}
					else if(ch == '\t')
						currentWidth += tabSize - (currentWidth % tabSize);
					else
						currentWidth++;
				}

				if(lastNewline != text.length())
				{
					totalLines++;
					lines.add(text.substring(lastNewline));
					lines.add(new Integer(currentWidth));
					maxWidth = Math.max(maxWidth,currentWidth);
				}
			} 

			
			int endOffset = 0;
			int lastLine = Math.max(startLine + totalLines - 1,endLine);
			for(int i = startLine; i <= lastLine; i++)
			{
				if(i == buffer.getLineCount())
					buffer.insert(buffer.getLength(),"\n");

				int lineStart = buffer.getLineStartOffset(i);
				int lineLen = buffer.getLineLength(i);

				int rectStart = buffer.getOffsetOfVirtualColumn(
					i,startColumn,total);
				int startWhitespace;
				if(rectStart == -1)
				{
					startWhitespace = (startColumn - total[0]);
					rectStart = lineLen;
				}
				else
					startWhitespace = 0;

				int rectEnd = buffer.getOffsetOfVirtualColumn(
					i,endColumn,null);
				if(rectEnd == -1)
					rectEnd = lineLen;

				buffer.remove(rectStart + lineStart,rectEnd - rectStart);

				if(startWhitespace != 0)
				{
					buffer.insert(rectStart + lineStart,
						MiscUtilities.createWhiteSpace(startWhitespace,0));
				}

				int endWhitespace;
				if(totalLines == 0)
				{
					if(rectEnd == lineLen)
						endWhitespace = 0;
					else
						endWhitespace = maxWidth - startColumn;
				}
				else
				{
					int index = 2 * ((i - startLine) % totalLines);
					String str = (String)lines.get(index);
					buffer.insert(rectStart + lineStart + startWhitespace,str);
					if(rectEnd == lineLen)
						endWhitespace = 0;
					else
					{
						endWhitespace = maxWidth
							- ((Integer)lines.get(index+1))
							.intValue();
					}
					startWhitespace += str.length();
				}

				if(endWhitespace != 0)
				{
					buffer.insert(rectStart + lineStart
						+ startWhitespace,
						MiscUtilities.createWhiteSpace(endWhitespace,0));
				}

				endOffset = rectStart + lineStart
					+ startWhitespace
					+ endWhitespace;
			} 

			
			if(text == null || text.length() == 0)
				return end;
			else
				return endOffset;
			
		} 

		
		boolean contentInserted(Buffer buffer, int startLine, int start,
			int numLines, int length)
		{
			if(this.end < start)
				return false;

			this.end += length;

			if(this.startLine > startLine)
			{
				this.start += length;
				if(numLines != 0)
				{
					this.startLine = buffer.getLineOfOffset(
						this.start);
				}
				return true;
			}

			int endVirtualColumn = buffer.getVirtualWidth(
				this.endLine,end
				- buffer.getLineStartOffset(this.endLine));

			if(this.start == start)
			{
				int startVirtualColumn = buffer.getVirtualWidth(
					this.startLine,start
					- buffer.getLineStartOffset(
					this.startLine));

				this.start += length;

				int newStartVirtualColumn
					= buffer.getVirtualWidth(
						startLine,start -
						buffer.getLineStartOffset(
						this.startLine));

				int[] totalVirtualWidth = new int[1];
				int newEnd = buffer.getOffsetOfVirtualColumn(
					this.endLine,endVirtualColumn +
					newStartVirtualColumn -
					startVirtualColumn,
					totalVirtualWidth);

				if(newEnd != -1)
				{
					end = buffer.getLineStartOffset(
						this.endLine) + newEnd;
				}
				else
				{
					end = buffer.getLineEndOffset(
						this.endLine) - 1;
					extraEndVirt = totalVirtualWidth[0]
						- endVirtualColumn;
				}
			}
			else if(this.start > start)
			{
				this.start += length;
				if(numLines != 0)
				{
					this.startLine = buffer.getLineOfOffset(
						this.start);
				}
			}

			if(numLines != 0)
				this.endLine = buffer.getLineOfOffset(this.end);
			int newEndVirtualColumn = buffer.getVirtualWidth(
				endLine,
				end - buffer.getLineStartOffset(this.endLine));
			if(startLine == this.endLine && extraEndVirt != 0)
			{
				extraEndVirt += (endVirtualColumn
					- newEndVirtualColumn);
			}
			else if(startLine == this.startLine
				&& extraStartVirt != 0)
			{
				extraStartVirt += (endVirtualColumn
					- newEndVirtualColumn);
			}

			return true;
		} 

		
		boolean contentRemoved(Buffer buffer, int startLine, int start,
			int numLines, int length)
		{
			int end = start + length;
			boolean changed = false;

			if(this.start > start && this.start <= end)
			{
				this.start = start;
				changed = true;
			}
			else if(this.start > end)
			{
				this.start -= length;
				changed = true;
			}

			if(this.end > start && this.end <= end)
			{
				this.end = start;
				changed = true;
			}
			else if(this.end > end)
			{
				this.end -= length;
				changed = true;
			}

			if(changed && numLines != 0)
			{
				this.startLine = buffer.getLineOfOffset(this.start);
				this.endLine = buffer.getLineOfOffset(this.end);
			}

			return changed;
		} 

		

		

		
		private int getColumnOnOtherLine(Buffer buffer, int line,
			int col)
		{
			int returnValue = buffer.getOffsetOfVirtualColumn(
				line,col,null);
			if(returnValue == -1)
				return buffer.getLineEndOffset(line) - 1;
			else
				return buffer.getLineStartOffset(line) + returnValue;
		} 

		
	} 
}
