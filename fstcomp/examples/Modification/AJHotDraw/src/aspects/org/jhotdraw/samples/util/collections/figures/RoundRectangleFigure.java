
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
public  class  RoundRectangleFigure  extends AttributeFigure {
		private Rectangle fDisplayBox;

		private int fArcWidth;

		private int fArcHeight;

		private static final int DEFAULT_ARC = 8;

		private static final long serialVersionUID = 7907900248924036885L;

		private int roundRectangleSerializedDataVersion = 1;

		public RoundRectangleFigure() {	this(new Point(0,0), new Point(0,0));	fArcWidth = fArcHeight = DEFAULT_ARC;	}

		public RoundRectangleFigure(Point origin, Point corner) {	basicDisplayBox(origin,corner);	fArcWidth = fArcHeight = DEFAULT_ARC;	}

		public void basicDisplayBox(Point origin, Point corner) {	fDisplayBox = new Rectangle(origin);	fDisplayBox.add(corner);	}

		public void setArc(int width, int height) {	willChange();	fArcWidth = width;	fArcHeight = height;	changed();	}

		public Point getArc() {	return new Point(fArcWidth, fArcHeight);	}

		public HandleEnumeration handles() {	List handles = CollectionsFactory.current().createList();	BoxHandleKit.addHandles(this, handles);	handles.add(new RadiusHandle(this));	return new HandleEnumerator(handles);	}

		public Rectangle displayBox() {	return new Rectangle(	fDisplayBox.x,	fDisplayBox.y,	fDisplayBox.width,	fDisplayBox.height);	}

		protected void basicMoveBy(int x, int y) {	fDisplayBox.translate(x,y);	}

		public void drawBackground(Graphics g) {	Rectangle r = displayBox();	g.fillRoundRect(r.x, r.y, r.width, r.height, fArcWidth, fArcHeight);	}

		public void drawFrame(Graphics g) {	Rectangle r = displayBox();	g.drawRoundRect(r.x, r.y, r.width-1, r.height-1, fArcWidth, fArcHeight);	}

		public Insets connectionInsets() {	return new Insets(fArcHeight/2, fArcWidth/2, fArcHeight/2, fArcWidth/2);	}

		public Connector connectorAt(int x, int y) {	return new ShortestDistanceConnector(this);	}


}
