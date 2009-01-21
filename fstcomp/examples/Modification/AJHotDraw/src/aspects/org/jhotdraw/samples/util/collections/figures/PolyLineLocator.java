
package org.jhotdraw.figures; 
import java.awt.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
 
class  PolyLineLocator  extends AbstractLocator {
		int fIndex;

		public PolyLineLocator(int index) {	fIndex = index;	}

		public Point locate(Figure owner) {	PolyLineFigure plf = (PolyLineFigure)owner;	if (fIndex < plf.pointCount()) {	return ((PolyLineFigure)owner).pointAt(fIndex);	}	return new Point(0, 0);	}


}
