
package org.jhotdraw.figures; 
import java.awt.Graphics; 
import java.awt.Point; 
import java.awt.Rectangle; 
import java.util.List; 
import org.jhotdraw.framework.HandleEnumeration; 
import org.jhotdraw.standard.BoxHandleKit; 
import org.jhotdraw.standard.HandleEnumerator; 
import org.jhotdraw.util.CollectionsFactory; 
public  class  RectangleFigure  extends AttributeFigure {
		private Rectangle fDisplayBox;

		private static final long serialVersionUID = 184722075881789163L;

		private int rectangleFigureSerializedDataVersion = 1;

		public RectangleFigure() {	this(new Point(0,0), new Point(0,0));	}

		public RectangleFigure(Point origin, Point corner) {	basicDisplayBox(origin,corner);	}

		public void basicDisplayBox(Point origin, Point corner) {	fDisplayBox = new Rectangle(origin);	fDisplayBox.add(corner);	}

		public HandleEnumeration handles() {	List handles = CollectionsFactory.current().createList();	BoxHandleKit.addHandles(this, handles);	return new HandleEnumerator(handles);	}

		public Rectangle displayBox() {	return new Rectangle(	fDisplayBox.x,	fDisplayBox.y,	fDisplayBox.width,	fDisplayBox.height);	}

		protected void basicMoveBy(int x, int y) {	fDisplayBox.translate(x,y);	}

		public void drawBackground(Graphics g) {	Rectangle r = displayBox();	g.fillRect(r.x, r.y, r.width, r.height);	}

		public void drawFrame(Graphics g) {	Rectangle r = displayBox();	g.drawRect(r.x, r.y, r.width-1, r.height-1);	}


}
