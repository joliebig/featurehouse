

package org.gjt.sp.jedit.textarea;

import java.awt.Graphics2D;

public class CircleFoldPainter extends ShapedFoldPainter
{
	@Override
	protected void paintFoldShape(Graphics2D gfx, int top, int bottom)
	{
		gfx.drawArc(1,top,8,(bottom - top),0,360);
	}

}
