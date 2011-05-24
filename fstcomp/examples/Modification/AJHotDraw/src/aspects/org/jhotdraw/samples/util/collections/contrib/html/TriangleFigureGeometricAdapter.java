
package org.jhotdraw.contrib.html; 
import java.awt.Point; 
import java.awt.Shape; 
import org.jhotdraw.contrib.TriangleFigure; 
public  class  TriangleFigureGeometricAdapter  extends TriangleFigure  implements GeometricFigure {
		public TriangleFigureGeometricAdapter() {	super();	}

		public TriangleFigureGeometricAdapter(Point origin, Point corner) {	super(origin, corner);	}

		public Shape getShape() {	return getPolygon();	}


}
