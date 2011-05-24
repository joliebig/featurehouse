
package org.jhotdraw.contrib.html; 
import java.awt.Point; 
import java.awt.Shape; 
import org.jhotdraw.contrib.DiamondFigure; 
public  class  DiamondFigureGeometricAdapter  extends DiamondFigure  implements GeometricFigure {
		public DiamondFigureGeometricAdapter() {	super();	}

		public DiamondFigureGeometricAdapter(Point origin, Point corner) {	super(origin, corner);	}

		public Shape getShape() {	return getPolygon();	}


}
