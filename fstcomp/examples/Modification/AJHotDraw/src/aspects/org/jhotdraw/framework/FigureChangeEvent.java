
package org.jhotdraw.framework; 
import java.awt.Rectangle; 
import java.util.EventObject; 
public  class  FigureChangeEvent  extends EventObject {
		private Rectangle myRectangle;

		private FigureChangeEvent myNestedEvent;

		private static final Rectangle EMPTY_RECTANGLE = new Rectangle(0, 0, 0, 0);

		public FigureChangeEvent(Figure newSource, Rectangle newRect) {	super(newSource);	myRectangle = newRect;	}

		public FigureChangeEvent(Figure newSource) {	super(newSource);	myRectangle = EMPTY_RECTANGLE;	}

		public FigureChangeEvent(Figure newSource, Rectangle newRect, FigureChangeEvent nestedEvent) {	this(newSource, newRect);	myNestedEvent = nestedEvent;	}

		public Figure getFigure() {	return (Figure)getSource();	}

		public Rectangle getInvalidatedRectangle() {	return myRectangle;	}

		public FigureChangeEvent getNestedEvent() {	return myNestedEvent;	}


}
