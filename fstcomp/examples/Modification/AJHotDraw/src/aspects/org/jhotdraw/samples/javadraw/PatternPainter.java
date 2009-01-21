
package org.jhotdraw.samples.javadraw; 
import java.awt.*; 
import org.jhotdraw.framework.*; 
public  class  PatternPainter 	implements Painter {
		private Image fImage;

		public PatternPainter(Image image) {	fImage = image;	}

		public void draw(Graphics g, DrawingView view) {	drawPattern(g, fImage, view);	}

		private void drawPattern(Graphics g, Image image, DrawingView view) {	int iwidth = image.getWidth(view);	int iheight = image.getHeight(view);	Dimension d = view.getSize();	int x = 0;	int y = 0;	while (y < d.height) {	while (x < d.width) {	g.drawImage(image, x, y, view);	x += iwidth;	}	y += iheight;	x = 0;	}	}


}
