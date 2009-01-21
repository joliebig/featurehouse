
package org.jhotdraw.standard; 
import java.awt.Color; 
import java.awt.Graphics; 
import java.awt.Point; 
import java.awt.Rectangle; 
import org.jhotdraw.framework.Connector; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.Locator; 
public  class  LocatorConnector  extends AbstractConnector {
		public static final int SIZE = 8;

		private Locator myLocator;

		private static final long serialVersionUID = 5062833203337604181L;

		private int locatorConnectorSerializedDataVersion = 1;

		public LocatorConnector() {	setLocator(null);	}

		public LocatorConnector(Figure owner, Locator l) {	super(owner);	setLocator(l);	}

		public boolean containsPoint(int x, int y) {	return displayBox().contains(x, y);	}

		public Rectangle displayBox() {	Point p = getLocator().locate(owner());	return new Rectangle(	p.x - SIZE / 2,	p.y - SIZE / 2,	SIZE,	SIZE);	}

		public void draw(Graphics g) {	Rectangle r = displayBox();	g.setColor(Color.blue);	g.fillOval(r.x, r.y, r.width, r.height);	g.setColor(Color.black);	g.drawOval(r.x, r.y, r.width, r.height);	}

		protected void setLocator(Locator newLocator) {	myLocator = newLocator;	}

		public Locator getLocator() {	return myLocator;	}


}
