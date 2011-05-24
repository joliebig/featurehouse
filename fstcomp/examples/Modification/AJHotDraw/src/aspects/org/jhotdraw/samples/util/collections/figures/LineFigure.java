
package org.jhotdraw.figures; 
import java.awt.*; 
public  class  LineFigure  extends PolyLineFigure {
		private static final long serialVersionUID = 511503575249212371L;

		private int lineFigureSerializedDataVersion = 1;

		public LineFigure() {	addPoint(0, 0);	addPoint(0, 0);	}

		public Point startPoint() {	return pointAt(0);	}

		public Point endPoint() {	return pointAt(1);	}

		public void startPoint(int x, int y) {	setPointAt(new Point(x,y), 0);	}

		public void endPoint(int x, int y) {	setPointAt(new Point(x,y), 1);	}

		public void setPoints(Point start, Point end) {	setPointAt(start, 0);	setPointAt(end, 1);	}

		public void basicDisplayBox(Point origin, Point corner) {	setPoints(origin, corner);	}


}
