
package org.jhotdraw.figures; 
import java.awt.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.Geom; 
public  class  ChopEllipseConnector  extends ChopBoxConnector {
		private static final long serialVersionUID = -3165091511154766610L;

		public ChopEllipseConnector() {	}

		public ChopEllipseConnector(Figure owner) {	super(owner);	}

		protected Point chop(Figure target, Point from) {	Rectangle r = target.displayBox();	double angle = Geom.pointToAngle(r, from);	return Geom.ovalAngleToPoint(r, angle);	}


}
