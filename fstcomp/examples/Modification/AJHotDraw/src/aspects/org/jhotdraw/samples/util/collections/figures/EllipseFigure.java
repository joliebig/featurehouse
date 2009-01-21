
package org.jhotdraw.figures; 
import java.awt.Graphics; 
import java.awt.Insets; 
import java.awt.Point; 
import java.awt.Rectangle; 
import java.util.List; 
import org.jhotdraw.framework.Connector; 
import org.jhotdraw.framework.HandleEnumeration; 
import org.jhotdraw.standard.BoxHandleKit; 
import org.jhotdraw.standard.HandleEnumerator; 
import org.jhotdraw.util.CollectionsFactory; 
public  class  EllipseFigure  extends AttributeFigure {
		private Rectangle fDisplayBox;

		private static final long serialVersionUID = -6856203289355118951L;

		private int ellipseFigureSerializedDataVersion = 1;

		public EllipseFigure() {	this(new Point(0,0), new Point(0,0));	}

		public EllipseFigure(Point origin, Point corner) {	basicDisplayBox(origin,corner);	}

		public HandleEnumeration handles() {	List handles = CollectionsFactory.current().createList();	BoxHandleKit.addHandles(this, handles);	return new HandleEnumerator(handles);	}

		public void basicDisplayBox(Point origin, Point corner) {	fDisplayBox = new Rectangle(origin);	fDisplayBox.add(corner);	}

		public Rectangle displayBox() {	return new Rectangle(	fDisplayBox.x,	fDisplayBox.y,	fDisplayBox.width,	fDisplayBox.height);	}

		protected void basicMoveBy(int x, int y) {	fDisplayBox.translate(x,y);	}

		public void drawBackground(Graphics g) {	Rectangle r = displayBox();	g.fillOval(r.x, r.y, r.width, r.height);	}

		public void drawFrame(Graphics g) {	Rectangle r = displayBox();	g.drawOval(r.x, r.y, r.width-1, r.height-1);	}

		public Insets connectionInsets() {	Rectangle r = fDisplayBox;	int cx = r.width/2;	int cy = r.height/2;	return new Insets(cy, cx, cy, cx);	}

		public Connector connectorAt(int x, int y) {	return new ChopEllipseConnector(this);	}


}
