

package org.gjt.sp.jedit.textarea;

import java.awt.Graphics2D;


public class SquareFoldPainter extends ShapedFoldPainter
{
	@Override
	protected void paintFoldShape(Graphics2D gfx, int top, int bottom)
	{
		gfx.drawRect(1,top,8,(bottom-top));
	}

} 
