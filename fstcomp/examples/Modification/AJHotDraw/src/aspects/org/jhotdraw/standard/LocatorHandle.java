
package org.jhotdraw.standard; 
import java.awt.Point; 
import org.jhotdraw.framework.Cursor; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.Locator; 
public  class  LocatorHandle  extends AbstractHandle {
		private Locator fLocator;

		public LocatorHandle(Figure owner, Locator l) {	super(owner);	fLocator = l;	}

		public Locator getLocator() {	return fLocator;	}

		public Point locate() {	return fLocator.locate(owner());	}

		public Cursor getCursor() {	Cursor c = super.getCursor();	if (getLocator() instanceof RelativeLocator) {	RelativeLocator rl = (RelativeLocator) getLocator();	if (rl.equals( RelativeLocator.north())) {	c = new AWTCursor(java.awt.Cursor.N_RESIZE_CURSOR);	}	else if (rl.equals(RelativeLocator.northEast())) {	c = new AWTCursor(java.awt.Cursor.NE_RESIZE_CURSOR);	}	else if (rl.equals(RelativeLocator.east())) {	c = new AWTCursor(java.awt.Cursor.E_RESIZE_CURSOR);	}	else if (rl.equals(RelativeLocator.southEast())) {	c = new AWTCursor(java.awt.Cursor.SE_RESIZE_CURSOR);	}	else if (rl.equals(RelativeLocator.south())) {	c = new AWTCursor(java.awt.Cursor.S_RESIZE_CURSOR);	}	else if (rl.equals(RelativeLocator.southWest())) {	c = new AWTCursor(java.awt.Cursor.SW_RESIZE_CURSOR);	}	else if (rl.equals(RelativeLocator.west())) {	c = new AWTCursor(java.awt.Cursor.W_RESIZE_CURSOR);	}	else if (rl.equals(RelativeLocator.northWest())) {	c = new AWTCursor(java.awt.Cursor.NW_RESIZE_CURSOR);	}	}	return c;	}


}
