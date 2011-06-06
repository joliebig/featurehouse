

package org.gjt.sp.jedit.buffer;

import java.util.ArrayList;
import java.util.List;

import javax.swing.text.Segment;


public class IndentFoldHandler extends FoldHandler
{
	public IndentFoldHandler()
	{
		super("indent");
	}

	
	
	private int getLeadingWhitespaceWidth(Segment seg, int tabSize)
	{
		int offset = seg.offset;
		int count = seg.count;
		int whitespace = 0;

		for(int i = 0; i < count; i++)
		{
			switch(seg.array[offset + i])
			{
			case ' ':
				whitespace++;
				break;
			case '\t':
				whitespace += (tabSize - whitespace % tabSize);
				break;
			default:
				return whitespace;
			}
		}
		return (-1);
	}

	
	
	public int getFoldLevel(JEditBuffer buffer, int lineIndex, Segment seg)
	{
		int tabSize = buffer.getTabSize();
		
		int prevLevel = 0;
		for (int index = lineIndex; index < buffer.getLineCount(); index++)
		{
			buffer.getLineText(index,seg);
			int whitespace = getLeadingWhitespaceWidth(seg,tabSize);
			if(whitespace >= 0)	
				return (whitespace > prevLevel) ? whitespace : prevLevel;
			if(index == 0)
				return 0;
			if(index == lineIndex)
				prevLevel = buffer.getFoldLevel(lineIndex - 1);
		}
		
		
		return prevLevel;
	} 

	
	
	public List<Integer> getPrecedingFoldLevels(JEditBuffer buffer,
		int lineIndex, Segment seg, int lineFoldLevel)
	{
		List<Integer> precedingFoldLevels = new ArrayList<Integer>();
		int tabSize = buffer.getTabSize();
		int whitespace = 0;
		int index;
		
		for (index = lineIndex - 1; index > 0; index--)
		{
			buffer.getLineText(index,seg);
			whitespace = getLeadingWhitespaceWidth(seg,tabSize);
			if (whitespace >= 0)
				break;
		}
		int max = (lineFoldLevel > whitespace) ? lineFoldLevel : whitespace;
		for (index++; index < lineIndex; index++)
			precedingFoldLevels.add(Integer.valueOf(max));
		return precedingFoldLevels;
	}
	

}
