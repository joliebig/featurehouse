
package org.jhotdraw.figures; 
import java.awt.Color; 
import java.awt.Graphics; 
import java.awt.Polygon; 
import java.awt.Rectangle; 
public abstract  class  AbstractLineDecoration  implements LineDecoration {
		static final long serialVersionUID = 1577970039258356627L;

		private Color fFillColor;

		private Color fBorderColor;

		private transient Rectangle myBounds;

		public AbstractLineDecoration() {	}

		public void draw(Graphics g, int x1, int y1, int x2, int y2) {	Polygon p = outline(x1, y1, x2, y2);	myBounds = p.getBounds();	if (getFillColor() == null) {	g.fillPolygon(p.xpoints, p.ypoints, p.npoints);	}	else {	Color drawColor = g.getColor();	g.setColor(getFillColor());	g.fillPolygon(p.xpoints, p.ypoints, p.npoints);	g.setColor(drawColor);	}	if (getBorderColor() != getFillColor()) {	Color drawColor = g.getColor();	g.setColor(getBorderColor());	g.drawPolygon(p.xpoints, p.ypoints, p.npoints);	g.setColor(drawColor);	}	}

		public Rectangle displayBox() {	if (myBounds != null) {	return myBounds;	}	else {	return new Rectangle(0, 0);	}	}

		public abstract Polygon outline(int x1, int y1, int x2, int y2);

		public void setFillColor(Color fillColor) {	fFillColor = fillColor;	}

		public Color getFillColor() {	return fFillColor;	}

		public void setBorderColor(Color borderColor) {	fBorderColor = borderColor;	}

		public Color getBorderColor() {	return fBorderColor;	}


}
