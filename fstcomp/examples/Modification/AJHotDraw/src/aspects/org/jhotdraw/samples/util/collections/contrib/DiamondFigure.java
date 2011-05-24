
package org.jhotdraw.contrib; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.figures.*; 
import java.awt.*; 
public  class  DiamondFigure  extends RectangleFigure {
		public DiamondFigure() {	super(new Point(0,0), new Point(0,0));	}

		public DiamondFigure(Point origin, Point corner) {	super(origin,corner);	}

		protected Polygon getPolygon() {	Rectangle r = displayBox();	Polygon p = new Polygon();	p.addPoint(r.x, r.y+r.height/2);	p.addPoint(r.x+r.width/2, r.y);	p.addPoint(r.x+r.width, r.y+r.height/2);	p.addPoint(r.x+r.width/2, r.y+r.height);	return p;	}

		public void draw(Graphics g) {	Polygon p = getPolygon();	g.setColor(getFillColor());	g.fillPolygon(p);	g.setColor(getFrameColor());	g.drawPolygon(p);	}

		public Insets connectionInsets() {	Rectangle r = displayBox();	return new Insets(r.height/2, r.width/2, r.height/2, r.width/2);	}

		public boolean containsPoint(int x, int y) {	return getPolygon().contains(x, y);	}

		public Point chop(Point p) {	return PolygonFigure.chop(getPolygon(), p);	}

		public Connector connectorAt(int x, int y) {	return new ChopDiamondConnector(this);	}


}
