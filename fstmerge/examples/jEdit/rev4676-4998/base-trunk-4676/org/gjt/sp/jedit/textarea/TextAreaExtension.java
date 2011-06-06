

package org.gjt.sp.jedit.textarea;

import java.awt.Graphics2D;


public abstract class TextAreaExtension
{
	
	
	public void paintValidLine(Graphics2D gfx, int screenLine,
		int physicalLine, int start, int end, int y) {} 

	
	
	public void paintInvalidLine(Graphics2D gfx, int screenLine,
		int y) {} 

	
	
	public String getToolTipText(int x, int y)
	{
		return null;
	} 
}
