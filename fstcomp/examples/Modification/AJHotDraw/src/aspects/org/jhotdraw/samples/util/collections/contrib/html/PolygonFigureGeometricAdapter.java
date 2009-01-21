
package org.jhotdraw.contrib.html; 
import java.awt.Polygon; 
import java.awt.Shape; 
import org.jhotdraw.contrib.PolygonFigure; 
public  class  PolygonFigureGeometricAdapter  extends PolygonFigure  implements GeometricFigure {
		public PolygonFigureGeometricAdapter() {	super();	}

		public PolygonFigureGeometricAdapter(int x, int y) {	super(x, y);	}

		public PolygonFigureGeometricAdapter(Polygon p) {	super(p);	}

		public Shape getShape() {	return getInternalPolygon();	}


}
