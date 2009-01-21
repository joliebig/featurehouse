
package org.jhotdraw.figures; 
import java.awt.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
public  class  BorderDecorator  extends DecoratorFigure {
		private static final long serialVersionUID = 1205601808259084917L;

		private int borderDecoratorSerializedDataVersion = 1;

		private Point myBorderOffset;

		private Color myBorderColor;

		private Color myShadowColor;

		public BorderDecorator() {	}

		public BorderDecorator(Figure figure) {	super(figure);	}

		protected void initialize() {	setBorderOffset(new Point(3,3));	}

		public void setBorderOffset(Point newBorderOffset) {	myBorderOffset = newBorderOffset;	}

		public Point getBorderOffset() {	if (myBorderOffset == null) {	return new Point(0,0);	}	else {	return myBorderOffset;	}	}

		public void draw(Graphics g) {	Rectangle r = displayBox();	super.draw(g);	g.setColor(Color.white);	g.drawLine(r.x, r.y, r.x, r.y + r.height);	g.drawLine(r.x, r.y, r.x + r.width, r.y);	g.setColor(Color.gray);	g.drawLine(r.x + r.width, r.y, r.x + r.width, r.y + r.height);	g.drawLine(r.x , r.y + r.height, r.x + r.width, r.y + r.height);	}

		public Rectangle displayBox() {	Rectangle r = getDecoratedFigure().displayBox();	r.grow(getBorderOffset().x, getBorderOffset().y);	return r;	}

		public void figureInvalidated(FigureChangeEvent e) {	Rectangle rect = e.getInvalidatedRectangle();	rect.grow(getBorderOffset().x, getBorderOffset().y);	super.figureInvalidated(new FigureChangeEvent(this, rect, e));	}

		public Insets connectionInsets() {	Insets i = super.connectionInsets();	i.top -= getBorderOffset().y;	i.bottom -= getBorderOffset().y;	i.left -= getBorderOffset().x;	i.right -= getBorderOffset().x;	return i;	}


}
