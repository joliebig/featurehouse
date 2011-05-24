
package org.jhotdraw.standard; 
import java.awt.*; 
import org.jhotdraw.framework.*; 
public  class  NullHandle  extends LocatorHandle {
		protected Locator fLocator;

		public NullHandle(Figure owner, Locator locator) {	super(owner, locator);	}

		public void draw(Graphics g) {	Rectangle r = displayBox();	g.setColor(Color.black);	g.drawRect(r.x, r.y, r.width, r.height);	}


}
