
package org.jhotdraw.contrib; 
import java.awt.Point; 
import java.awt.Rectangle; // JUnitDoclet begin import
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.FigureEnumeration; 
public  class  StandardLayouter  extends SimpleLayouter {
		public StandardLayouter() {	this(null);	}

		public StandardLayouter(Layoutable newLayoutable) {	super(newLayoutable);	}

		public Layouter create(Layoutable newLayoutable) {	return new StandardLayouter(newLayoutable);	}

		public Rectangle calculateLayout(Point origin, Point corner) {	int maxWidth = Math.abs(corner.x - origin.x);	int maxHeight = getInsets().top;	FigureEnumeration fe = getLayoutable().figures();	while (fe.hasNextFigure()) {	Figure currentFigure = fe.nextFigure();	Rectangle r = null;	if (currentFigure instanceof Layoutable) {	Layouter layoutStrategy = ((Layoutable)currentFigure).getLayouter();	r = layoutStrategy.calculateLayout(	new Point(0, 0), new Point(0, 0));	}	else {	r = new Rectangle(currentFigure.displayBox().getBounds());	}	maxWidth = Math.max(maxWidth, r.width + getInsets().left + getInsets().right);	maxHeight += r.height;	}	maxHeight += getInsets().bottom;	return new Rectangle(origin.x, origin.y, maxWidth, maxHeight);	}

		public Rectangle layout(Point origin, Point corner) {	Rectangle r = calculateLayout(origin, corner);	int maxHeight = getInsets().top;	FigureEnumeration fe = getLayoutable().figures();	while (fe.hasNextFigure()) {	Figure currentFigure = fe.nextFigure();	Point partOrigin = new Point(r.x + getInsets().left, r.y + maxHeight);	Point partCorner = new Point	(r.x + r.width - getInsets().right, r.y + maxHeight + currentFigure.displayBox().height);	currentFigure.displayBox(partOrigin, partCorner);	maxHeight += currentFigure.displayBox().height;	}	return new Rectangle(r.x, r.y, r.width, maxHeight + getInsets().bottom);	}


}
