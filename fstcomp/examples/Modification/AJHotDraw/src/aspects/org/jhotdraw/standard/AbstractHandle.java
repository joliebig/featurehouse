
package org.jhotdraw.standard; 
import java.awt.Color; 
import java.awt.Graphics; 
import java.awt.Point; 
import java.awt.Rectangle; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.Undoable; 
public abstract  class  AbstractHandle  implements Handle {
		public static final int HANDLESIZE = 8;

		private Figure fOwner;

		private Undoable myUndoableActivity;

		public AbstractHandle(Figure owner) {	fOwner = owner;	}

		public void invokeStart(int x, int y, DrawingView view) {	invokeStart(x, y, view.drawing());	}

		public void invokeStart(int x, int y, Drawing drawing) { }

		public void invokeStep(int x, int y, int anchorX, int anchorY, DrawingView view) {	invokeStep(x-anchorX, y-anchorY, view.drawing());	}

		public void invokeStep(int dx, int dy, Drawing drawing) { }

		public void invokeEnd(int x, int y, int anchorX, int anchorY, DrawingView view) {	invokeEnd(x-anchorX, y-anchorY, view.drawing());	}

		public void invokeEnd(int dx, int dy, Drawing drawing) { }

		public Figure owner() {	return fOwner;	}

		public Rectangle displayBox() {	Point p = locate();	return new Rectangle(	p.x - HANDLESIZE / 2,	p.y - HANDLESIZE / 2,	HANDLESIZE,	HANDLESIZE);	}

		public boolean containsPoint(int x, int y) {	return displayBox().contains(x, y);	}

		public void draw(Graphics g) {	Rectangle r = displayBox();	g.setColor(Color.white);	g.fillRect(r.x, r.y, r.width, r.height);	g.setColor(Color.black);	g.drawRect(r.x, r.y, r.width, r.height);	}

		public Undoable getUndoActivity() {	return myUndoableActivity;	}

		public void setUndoActivity(Undoable newUndoableActivity) {	myUndoableActivity = newUndoableActivity;	}

		public Cursor getCursor() {	return new AWTCursor(AWTCursor.DEFAULT_CURSOR);	}


}
