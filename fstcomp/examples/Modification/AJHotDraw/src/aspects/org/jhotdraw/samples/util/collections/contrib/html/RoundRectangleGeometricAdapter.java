
package org.jhotdraw.contrib.html; 
import java.awt.Point; 
import java.awt.Rectangle; 
import java.awt.Shape; 
import java.awt.geom.RoundRectangle2D; 
import org.jhotdraw.figures.RoundRectangleFigure; 
public  class  RoundRectangleGeometricAdapter  extends RoundRectangleFigure  implements GeometricFigure {
		public RoundRectangleGeometricAdapter() {	super();	}

		public RoundRectangleGeometricAdapter(Point origin, Point corner) {	super(origin, corner);	}

		public Shape getShape() {	Point arc = getArc();	Rectangle dspBox = displayBox();	RoundRectangle2D.Float roundRectangle = new RoundRectangle2D.Float(	dspBox.x, dspBox.y, dspBox.width, dspBox.height,	arc.x, arc.y);	return roundRectangle;	}


}
