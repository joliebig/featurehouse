
package org.jhotdraw.figures; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.Geom; 
import java.awt.*; 
public  class  ShortestDistanceConnector  extends AbstractConnector {
		private static final long serialVersionUID = -2273446020593433887L;

		public ShortestDistanceConnector() {	super();	}

		public ShortestDistanceConnector(Figure owner) {	super(owner);	}

		public Point findStart(ConnectionFigure connection) {	return findPoint(connection, true);	}

		public Point findEnd(ConnectionFigure connection) {	return findPoint(connection, false);	}

		protected Point findPoint(ConnectionFigure connection, boolean getStart) {	Figure startFigure = connection.getStartConnector().owner();	Figure endFigure = connection.getEndConnector().owner();	Rectangle r1 = startFigure.displayBox();	Rectangle r2 = endFigure.displayBox();	Insets i1 = startFigure.connectionInsets();	Insets i2 = endFigure.connectionInsets();	Point p1, p2;	Point start = null, end = null, s = null, e = null;	long len2 = Long.MAX_VALUE, l2;	int x1, x2, y1, y2;	int xmin, xmax, ymin, ymax;	int r1x, r1width, r2x, r2width, r1y, r1height, r2y, r2height;	r1x = r1.x + i1.left;	r1width = r1.width - i1.left - i1.right-1;	r2x = r2.x + i2.left;	r2width = r2.width - i2.left - i2.right-1;	if (r1x + r1width < r2x) {	x1 = r1x + r1width;	x2 = r2x;	}	else if (r1x > r2x + r2width) {	x1 = r1x;	x2 = r2x + r2width;	}	else {	xmax = Math.max(r1x, r2x);	xmin = Math.min(r1x+r1width, r2x+r2width);	x1 = x2 = (xmax + xmin) /2;	}	r1y = r1.y + i1.top;	r1height = r1.height - i1.top - i1.bottom-1;	r2y = r2.y + i2.top;	r2height = r2.height - i2.top - i2.bottom-1;	if (r1y + r1height < r2y) {	y1 = r1y + r1height;	y2 = r2y;	}	else if (r1y > r2y + r2height) {	y1 = r1y;	y2 = r2y + r2height;	}	else {	ymax = Math.max(r1y, r2y);	ymin = Math.min(r1y+r1height, r2y+r2height);	y1 = y2 = (ymax + ymin) /2;	}	for (int i = 0; i < 4; i++) {	switch(i) {	case 0:	p1 = Geom.east(r1);	p2 = Geom.west(r2);	s = new Point(p1.x, y1);	e = new Point(p2.x, y2);	break;	case 1:	p1 = Geom.west(r1);	p2 = Geom.east(r2);	s = new Point(p1.x, y1);	e = new Point(p2.x, y2);	break;	case 2:	p1 = Geom.north(r1);	p2 = Geom.south(r2);	s = new Point(x1, p1.y);	e = new Point(x2, p2.y);	break;	case 3:	p1 = Geom.south(r1);	p2 = Geom.north(r2);	s = new Point(x1, p1.y);	e = new Point(x2, p2.y);	break;	}	l2 = Geom.length2(s.x, s.y, e.x, e.y);	if (l2 < len2) {	start = s;	end = e;	len2 = l2;	}	}	if (getStart) {	return start;	}	return end;	}


}
