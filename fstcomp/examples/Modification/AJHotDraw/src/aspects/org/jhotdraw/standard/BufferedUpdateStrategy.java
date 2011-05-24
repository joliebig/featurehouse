
package org.jhotdraw.standard; 
import java.awt.*; 
import org.jhotdraw.framework.*; 
public  class  BufferedUpdateStrategy  implements Painter {
		transient private Image fOffscreen;

		private int fImagewidth = -1;

		private int fImageheight = -1;

		private static final long serialVersionUID = 6489532222954612824L;

		private int bufferedUpdateSerializedDataVersion = 1;

		public void draw(Graphics g, DrawingView view) {	Dimension d = view.getSize();	if ((fOffscreen == null) || (d.width != fImagewidth)	|| (d.height != fImageheight)) {	fOffscreen = view.createImage(d.width, d.height);	fImagewidth = d.width;	fImageheight = d.height;	}	Graphics g2 = fOffscreen.getGraphics();	view.drawAll(g2);	g.drawImage(fOffscreen, 0, 0, view);	}


}
