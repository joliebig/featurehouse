
package org.jhotdraw.figures; 
import java.awt.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.*; 
public  class  PolyLineConnector  extends ChopBoxConnector {
		private static final long serialVersionUID = 6018435940519102865L;

		public PolyLineConnector() {	super();	}

		public PolyLineConnector(Figure owner) {	super(owner);	}

		protected Point chop(Figure target, Point from) {	PolyLineFigure p = (PolyLineFigure)owner();	Point ctr = p.center();	int cx = -1;	int cy = -1;	long len = Long.MAX_VALUE;	for (int i = 0; i < p.pointCount()-1; i++) {	Point p1 = p.pointAt(i);	Point p2 = p.pointAt(i+1);	Point chop = Geom.intersect(p1.x, p1.y, p2.x, p2.y, from.x, from.y, ctr.x, ctr.y);	if (chop != null) {	long cl = Geom.length2(chop.x, chop.y, from.x, from.y);	if (cl < len) {	len = cl;	cx = chop.x;	cy = chop.y;	}	}	}	{	for (int i = 0; i < p.pointCount(); i++) {	Point pp = p.pointAt(i);	long l = Geom.length2(pp.x, pp.y, from.x, from.y);	if (l < len) {	len = l;	cx = pp.x;	cy = pp.y;	}	}	}	return new Point(cx, cy);	}


}
