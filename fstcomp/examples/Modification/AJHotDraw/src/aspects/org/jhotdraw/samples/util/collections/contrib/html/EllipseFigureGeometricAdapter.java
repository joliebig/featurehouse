
package org.jhotdraw.contrib.html; 
import java.awt.Point; 
import java.awt.Rectangle; 
import java.awt.Shape; 
import java.awt.geom.Ellipse2D; 
import org.jhotdraw.figures.EllipseFigure; 
public  class  EllipseFigureGeometricAdapter  extends EllipseFigure  implements GeometricFigure {
		public EllipseFigureGeometricAdapter() {	super();	}

		public EllipseFigureGeometricAdapter(Point origin, Point corner) {	super(origin, corner);	}

		public Shape getShape() {	Rectangle rect = displayBox();	Ellipse2D.Float ellipse = new Ellipse2D.Float(rect.x, rect.y, rect.width, rect.height);	return ellipse;	}


}
