
package org.jhotdraw.contrib; 
import java.awt.Insets; 
import java.awt.Point; 
import java.awt.Rectangle; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.FigureEnumeration; 
public  class  SimpleLayouter  implements Layouter {
		private Layoutable myLayoutable;

		private Insets myInsets;

		static final long serialVersionUID = 2928651014089117493L;

		private SimpleLayouter() {	}

		public SimpleLayouter(Layoutable newLayoutable) {	setLayoutable(newLayoutable);	setInsets(new Insets(0, 0, 0, 0));	}

		public Layoutable getLayoutable() {	return myLayoutable;	}

		public void setLayoutable(Layoutable newLayoutable) {	myLayoutable = newLayoutable;	}

		public void setInsets(Insets newInsets) {	myInsets = newInsets;	}

		public Insets getInsets() {	return myInsets;	}

		public Layouter create(Layoutable newLayoutable) {	SimpleLayouter newLayouter = new SimpleLayouter(newLayoutable);	newLayouter.setInsets((Insets)getInsets().clone());	return newLayouter;	}

		public Rectangle calculateLayout(Point origin, Point corner) {	Rectangle maxRect = new Rectangle(origin);	maxRect.add(corner);	FigureEnumeration fe = getLayoutable().figures();	while (fe.hasNextFigure()) {	Figure currentFigure = fe.nextFigure();	maxRect.union(currentFigure.displayBox());	}	maxRect.width += getInsets().left + getInsets().right;	maxRect.height += getInsets().top + getInsets().bottom;	return maxRect;	}

		public Rectangle layout(Point origin, Point corner) {	return calculateLayout(origin, corner);	}


}
