

package org.gjt.sp.jedit.gui.statusbar;

import java.awt.Point;
import java.awt.event.MouseEvent;
import javax.swing.JLabel;


public class ToolTipLabel extends JLabel
{
	
	@Override
	public Point getToolTipLocation(MouseEvent event)
	{
		return new Point(event.getX(),-20);
	} 
}
