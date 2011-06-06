

package org.gjt.sp.jedit.buffer;

import javax.swing.text.Segment;
import org.gjt.sp.jedit.Buffer;


public class ExplicitFoldHandler extends FoldHandler
{
	
	public ExplicitFoldHandler()
	{
		super("explicit");
	} 

	
	
	public int getFoldLevel(Buffer buffer, int lineIndex, Segment seg)
	{
		if(lineIndex == 0)
			return 0;
		else
		{
			int foldLevel = buffer.getFoldLevel(lineIndex - 1);

			buffer.getLineText(lineIndex - 1,seg);

			int offset = seg.offset;
			int count = seg.count;

			int openingBrackets = 0, closingBrackets = 0;
			for(int i = 0; i < count; i++)
			{
				switch(seg.array[offset + i])
				{
				case '{':
					closingBrackets = 0;
					openingBrackets++;
					if(openingBrackets == 3)
					{
						foldLevel++;
						openingBrackets = 0;
					}
					break;
				case '}':
					openingBrackets = 0;
					closingBrackets++;
					if(closingBrackets == 3)
					{
						if(foldLevel > 0)
							foldLevel--;
						closingBrackets = 0;
					}
					break;
				default:
					closingBrackets = openingBrackets = 0;
					break;
				}
			}

			return foldLevel;
		}
	} 
}
