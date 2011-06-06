

package org.gjt.sp.jedit.textarea;

import java.awt.Graphics2D;
import org.gjt.sp.jedit.buffer.JEditBuffer;


public class TriangleFoldPainter implements FoldPainter {

	
	public void paintFoldStart(Gutter gutter, Graphics2D gfx, int screenLine,
			int physicalLine, boolean nextLineVisible, int y, int lineHeight,
			JEditBuffer buffer)
	{
		int _y = y + lineHeight / 2;
		gfx.setColor(gutter.getFoldColor());
		if (nextLineVisible)
		{
			gfx.drawLine(1,_y - 3,10,_y - 3);
			gfx.drawLine(2,_y - 2,9,_y - 2);
			gfx.drawLine(3,_y - 1,8,_y - 1);
			gfx.drawLine(4,_y,7,_y);
			gfx.drawLine(5,_y + 1,6,_y + 1);
		}
		else
		{
			gfx.drawLine(4,_y - 5,4,_y + 4);
			gfx.drawLine(5,_y - 4,5,_y + 3);
			gfx.drawLine(6,_y - 3,6,_y + 2);
			gfx.drawLine(7,_y - 2,7,_y + 1);
			gfx.drawLine(8,_y - 1,8,_y);
		}
	} 

	
	public void paintFoldEnd(Gutter gutter, Graphics2D gfx, int screenLine,
			int physicalLine, int y, int lineHeight, JEditBuffer buffer)
	{
		gfx.setColor(gutter.getFoldColor());
		int _y = y + lineHeight / 2;
		gfx.drawLine(4,_y,4,_y + 3);
		gfx.drawLine(4,_y + 3,7,_y + 3);
	} 

	
	public void paintFoldMiddle(Gutter gutter, Graphics2D gfx, int screenLine,
			int physicalLine, int y, int lineHeight, JEditBuffer buffer)
	{
	} 

}
