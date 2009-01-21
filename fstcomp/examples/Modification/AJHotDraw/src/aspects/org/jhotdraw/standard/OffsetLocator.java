
package org.jhotdraw.standard; 
import java.awt.Point; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.Locator; 
public  class  OffsetLocator  extends AbstractLocator {
		private static final long serialVersionUID = 2679950024611847621L;

		private int offsetLocatorSerializedDataVersion = 1;

		private Locator fBase;

		private int fOffsetX;

		private int fOffsetY;

		public OffsetLocator() {	fBase = null;	fOffsetX = 0;	fOffsetY = 0;	}

		public OffsetLocator(Locator base) {	this();	fBase = base;	}

		public OffsetLocator(Locator base, int offsetX, int offsetY) {	this(base);	fOffsetX = offsetX;	fOffsetY = offsetY;	}

		public Point locate(Figure owner) {	Point p = fBase.locate(owner);	p.x += fOffsetX;	p.y += fOffsetY;	return p;	}

		public void moveBy(int dx, int dy) {	fOffsetX += dx;	fOffsetY += dy;	}


}
