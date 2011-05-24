
package org.jhotdraw.figures; 
import java.awt.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.NullHandle; 
final  class  GroupHandle  extends NullHandle {
		public GroupHandle(Figure owner, Locator locator) {	super(owner, locator);	}

		public void draw(Graphics g) {	Rectangle r = displayBox();	g.setColor(Color.black);	g.drawRect(r.x, r.y, r.width, r.height);	r.grow(-1, -1);	g.setColor(Color.white);	g.drawRect(r.x, r.y, r.width, r.height);	}


}
