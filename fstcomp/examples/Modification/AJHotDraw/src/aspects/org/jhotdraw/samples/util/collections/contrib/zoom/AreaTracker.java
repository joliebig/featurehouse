
package org.jhotdraw.contrib.zoom; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.standard.AbstractTool; 
import java.awt.*; 
import java.awt.event.MouseEvent; 
public abstract  class  AreaTracker  extends AbstractTool {
		private Rectangle area;

		protected AreaTracker(DrawingEditor editor) {	super(editor);	}

		public Rectangle getArea() {	return new Rectangle(area.x, area.y, area.width, area.height);	}

		public void mouseDown(MouseEvent e, int x, int y) {	super.mouseDown(e, e.getX(), e.getY());	rubberBand(getAnchorX(), getAnchorY(), getAnchorX(), getAnchorY());	}

		public void mouseDrag(MouseEvent e, int x, int y) {	super.mouseDrag(e, x, y);	eraseRubberBand();	rubberBand(getAnchorX(), getAnchorY(), x, y);	}

		public void mouseUp(MouseEvent e, int x, int y) {	super.mouseUp(e, x, y);	eraseRubberBand();	}

		private void rubberBand(int x1, int y1, int x2, int y2) {	area = new Rectangle(new Point(x1, y1));	area.add(new Point(x2, y2));	drawXORRect(area);	}

		private void eraseRubberBand() {	drawXORRect(area);	}

		private void drawXORRect(Rectangle r) {	Graphics g = view().getGraphics();	g.setXORMode(view().getBackground());	g.setColor(Color.black);	g.drawRect(r.x, r.y, r.width, r.height);	}


}
