
package org.jhotdraw.contrib; 
import java.awt.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.ChopBoxConnector; 
import org.jhotdraw.util.Geom; 
public  class  ChopDiamondConnector  extends ChopBoxConnector {
		private static final long serialVersionUID = -1461450322512395462L;

		public ChopDiamondConnector() {	}

	 public ChopDiamondConnector(Figure owner) { super(owner);	}

		protected Point chop(Figure target, Point from) {	Rectangle r = target.displayBox();	Point c1 = new Point(r.x + r.width/2, r.y + (r.height/2));	Point p2 = new Point(r.x + r.width/2, r.y + r.height);	Point p4 = new Point(r.x + r.width/2, r.y);	if (r.contains(from)) {	if (from.y > r.y && from.y < (r.y +r.height/2)) {	return p2;	}	else {	return p4;	}	}	double ang = Geom.pointToAngle(r, from);	Point p1 = new Point(r.x + r.width , r.y + (r.height/2));	Point p3 = new Point(r.x , r.y + (r.height/2));	Point rp = null;	if (ang > 0 && ang < 1.57) {	rp = Geom.intersect(p1.x, p1.y, p2.x, p2.y, c1.x, c1.y, from.x, from.y);	}	else if (ang > 1.575 && ang < 3.14) {	rp = Geom.intersect(p2.x, p2.y, p3.x, p3.y, c1.x, c1.y, from.x, from.y);	}	else if (ang > -3.14 && ang < -1.575) { rp = Geom.intersect(p3.x, p3.y, p4.x, p4.y, c1.x, c1.y, from.x, from.y);	}	else if (ang > -1.57 && ang < 0) {	rp = Geom.intersect(p4.x, p4.y, p1.x, p1.y, c1.x, c1.y, from.x, from.y);	}	if (rp == null) {	rp = Geom.angleToPoint(r, ang);	}	return rp;	}


}
