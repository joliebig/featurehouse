

package org.gjt.sp.jedit.textarea;

import java.awt.Graphics2D;


public abstract class TextAreaExtension
{
	
	
	public void paintScreenLineRange(Graphics2D gfx, int firstLine,
		int lastLine, int[] physicalLines, int[] start, int[] end,
		int y, int lineHeight)
	{
		for(int i = 0; i < physicalLines.length; i++)
		{
			int screenLine = i + firstLine;
			if(physicalLines[i] == -1)
				paintInvalidLine(gfx,screenLine,y);
			else
			{
				paintValidLine(gfx,screenLine,physicalLines[i],
					start[i],end[i],y);
			}

			y += lineHeight;
		}
	} 

	
	
	public void paintValidLine(Graphics2D gfx, int screenLine,
		int physicalLine, int start, int end, int y) {} 

	
	
	public void paintInvalidLine(Graphics2D gfx, int screenLine,
		int y) {} 

	
	
	public String getToolTipText(int x, int y)
	{
		return null;
	} 
}
