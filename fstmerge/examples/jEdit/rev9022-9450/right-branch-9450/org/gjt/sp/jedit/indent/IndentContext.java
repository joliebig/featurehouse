

package org.gjt.sp.jedit.indent;

import java.util.LinkedList;
import java.util.List;

import org.gjt.sp.jedit.TextUtilities;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.util.StandardUtilities;


public class IndentContext
{

	public IndentContext(JEditBuffer buffer, int line)
	{
		this.buffer = buffer;
		this.line = line;
		this.prevLineIndex = -1;
		this.prevPrevLineIndex = -1;
		this.oldIndent = -1;
	}

	
	public JEditBuffer getBuffer()
	{
		return buffer;
	} 

	
	public int getLineIndex(int offset)
	{
		assert (offset <= 0) : "positive offsets are not handled";
		int lIdx = line;
		for (int i = -1; i >= offset && lIdx >= 0; i--)
		{
			if (i == -1)
			{
				if (prevLineIndex == -1)
				{
					prevLineIndex = buffer.getPriorNonEmptyLine(lIdx);
				}
				lIdx = prevLineIndex;
			}
			else if (i == -2)
			{
				if (prevPrevLineIndex == -1)
				{
					prevPrevLineIndex = buffer.getPriorNonEmptyLine(lIdx);
				}
				lIdx = prevPrevLineIndex;
			}
			else
			{
				lIdx = buffer.getPriorNonEmptyLine(lIdx);
			}
		}
		return lIdx;
	} 

	
	
	public CharSequence getLineText(int offset)
	{
		int lIdx = getLineIndex(offset);
		if (lIdx < 0)
			return null;

		if (lIdx == line)
		{
			if (thisLine == null)
				thisLine = buffer.getLineSegment(line);
			return thisLine;
		}
		else if (lIdx == prevLineIndex)
		{
			if (prevLine == null)
				prevLine = buffer.getLineSegment(prevLineIndex);
			return prevLine;
		}
		else if (lIdx == prevPrevLineIndex)
		{
			if (prevPrevLine == null)
				prevPrevLine = buffer.getLineSegment(prevPrevLineIndex);
			return prevPrevLine;
		}
		else
		{
			return buffer.getLineSegment(lIdx);
		}
	} 

	
	public Brackets getBrackets(int offset, char open, char close)
	{
		return getBrackets(getLineText(offset), open, close);
	} 

	
	public Brackets getBrackets(CharSequence line, char open, char close)
	{
		Brackets brackets = new Brackets();

		for(int i = 0; i < line.length(); i++)
		{
			char ch = line.charAt(i);
			if(ch == open)
			{
				
				if(open == '{' && line.length() - i >= 3)
				{
					if(line.subSequence(i,i+3).equals("{{{")) 
					{
						i += 2;
						continue;
					}
				}
				brackets.openCount++;
			}
			else if(ch == close)
			{
				if(brackets.openCount != 0)
					brackets.openCount--;
				else
					brackets.closeCount++;
			}
		}

		return brackets;
	} 

	
	public int getOldIndent()
	{
		if (oldIndent == -1)
		{
			CharSequence prev = getLineText(-1);
			oldIndent = (prev == null) ? 0 :
				StandardUtilities.getLeadingWhiteSpaceWidth(
					prev, buffer.getTabSize());
		}
		return oldIndent;
	} 

	
	
	public List<IndentAction> getActions()
	{
		return actions;
	}

	
	public void addAction(IndentAction action)
	{
		if (actions == null)
		{
			actions = new LinkedList<IndentAction>();
		}
		actions.add(action);
	}
	

	
	public static class Brackets
	{
		int openCount;
		int closeCount;
	} 

	
	private int line;
	private JEditBuffer buffer;
	private List<IndentAction> actions;

	
	private int oldIndent;

	
	private CharSequence	thisLine;
	private int 		prevLineIndex;
	private CharSequence 	prevLine;
	private int 		prevPrevLineIndex;
	private CharSequence 	prevPrevLine;
	

}

