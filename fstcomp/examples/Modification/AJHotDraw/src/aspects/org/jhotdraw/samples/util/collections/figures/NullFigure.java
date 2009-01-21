
package org.jhotdraw.figures; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.FigureEnumerator; 
import org.jhotdraw.standard.SingleFigureEnumerator; 
import org.jhotdraw.standard.AbstractFigure; 
import org.jhotdraw.standard.HandleEnumerator; 
import java.awt.*; 
public  class  NullFigure  extends AbstractFigure {
		private Rectangle myDisplayBox;

		protected void basicMoveBy(int dx, int dy) {	myDisplayBox.translate(dx, dy);	}

		public void basicDisplayBox(Point origin, Point corner) {	myDisplayBox = new Rectangle(origin);	myDisplayBox.add(corner);	}

		public Rectangle displayBox() {	return new Rectangle(myDisplayBox);	}

		public void draw(Graphics g) {	}

		public HandleEnumeration handles() {	return HandleEnumerator.getEmptyEnumeration();	}

		public boolean isEmpty() {	return true;	}

		public FigureEnumeration figures() {	return FigureEnumerator.getEmptyEnumeration();	}

		public Figure findFigureInside(int x, int y) {	return null;	}

		public Object clone() {	return super.clone();	}

		public boolean includes(Figure figure) {	return false;	}

		public FigureEnumeration decompose() {	return new SingleFigureEnumerator(this);	}

		public void release() {	}

		public void invalidate() {	}

		public Object getAttribute(String name) {	return null;	}

		public Object getAttribute(FigureAttributeConstant attributeConstant) {	return null;	}

		public void setAttribute(String name, Object value) {	}

		public void setAttribute(FigureAttributeConstant attributeConstant, Object value) {	}


}
