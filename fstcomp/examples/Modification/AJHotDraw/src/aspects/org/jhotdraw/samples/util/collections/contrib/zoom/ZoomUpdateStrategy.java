
package org.jhotdraw.contrib.zoom; 
import org.jhotdraw.framework.DrawingView; 
import org.jhotdraw.framework.Painter; 
import java.awt.*; 
public  class  ZoomUpdateStrategy  implements Painter {
		transient private Image fOffscreen;

		private int fImagewidth = -1;

		private int fImageheight = -1;

		public void draw(Graphics g, DrawingView view) {	Dimension d = view.getSize();	if ((fOffscreen == null) || (d.width != fImagewidth)	|| (d.height != fImageheight)) {	fOffscreen = view.createImage(d.width, d.height);	fImagewidth = d.width;	fImageheight = d.height;	}	Graphics g2 = fOffscreen.getGraphics();	Rectangle r = g.getClipBounds();	if (g2 instanceof ScalingGraphics) {	ScalingGraphics s2 = (ScalingGraphics) g2;	if (r != null) {	r = new Rectangle((int) ((r.x - 2) / s2.getScale()),	(int) ((r.y - 2) / s2.getScale()),	(int) ((r.width + 4) / s2.getScale()),	(int) ((r.height + 4) / s2.getScale()));	g.setClip(r);	}	}	g2.setClip(r);	view.drawAll(g2);	g.drawImage(fOffscreen, 0, 0, view);	}


}
