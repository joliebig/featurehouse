

package org.gjt.sp.jedit.buffer;

import javax.swing.text.Segment;


public class IndentFoldHandler extends FoldHandler
{
	public IndentFoldHandler()
	{
		super("indent");
	}

	
	
	public int getFoldLevel(JEditBuffer buffer, int lineIndex, Segment seg)
	{
		int tabSize = buffer.getTabSize();

		buffer.getLineText(lineIndex,seg);

		int offset = seg.offset;
		int count = seg.count;

		int whitespace = 0;

		boolean seenNonWhiteSpace = false;

loop:		for(int i = 0; i < count; i++)
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
				seenNonWhiteSpace = true;
				break loop;
			}
		}

		if(!seenNonWhiteSpace)
		{
			
			if(lineIndex != 0)
				return buffer.getFoldLevel(lineIndex - 1);
			else
				return 0;
		}
			return whitespace;
	} 
}
