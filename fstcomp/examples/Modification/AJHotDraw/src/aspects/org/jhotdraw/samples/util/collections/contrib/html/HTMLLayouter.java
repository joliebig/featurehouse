
package org.jhotdraw.contrib.html; 
import java.awt.Insets; 
import java.awt.Point; 
import java.awt.Rectangle; 
import org.jhotdraw.contrib.Layoutable; 
import org.jhotdraw.contrib.Layouter; 
public  class  HTMLLayouter  implements Layouter {
		public HTMLLayouter() { }

		public HTMLLayouter(Layoutable newLayoutable) {	this();	}

		public Rectangle calculateLayout(Point origin, Point corner) {	throw new UnsupportedOperationException("Method calculateLayout() not yet implemented.");	}

		public Rectangle layout(Point origin, Point corner) {	throw new UnsupportedOperationException("Method layout() not yet implemented.");	}

		public void setInsets(Insets newInsets) {	throw new UnsupportedOperationException("Method setInsets() not yet implemented.");	}

		public Insets getInsets() {	throw new UnsupportedOperationException("Method getInsets() not yet implemented.");	}

		public Layouter create(Layoutable newLayoutable) {	return new HTMLLayouter(newLayoutable);	}


}
