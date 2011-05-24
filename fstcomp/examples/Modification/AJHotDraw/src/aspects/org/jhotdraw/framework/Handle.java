
package org.jhotdraw.framework; 
import java.awt.Graphics; 
import java.awt.Point; 
import java.awt.Rectangle; 
import org.jhotdraw.util.Undoable; 
public  interface  Handle {
		public static final int HANDLESIZE = 8;

		public Point locate();

		public void invokeStart(int x, int y, DrawingView view);

		public void invokeStart(int x, int y, Drawing drawing);

		public void invokeStep (int x, int y, int anchorX, int anchorY, DrawingView view);

		public void invokeStep (int dx, int dy, Drawing drawing);

		public void invokeEnd(int x, int y, int anchorX, int anchorY, DrawingView view);

		public void invokeEnd(int dx, int dy, Drawing drawing);

		public Figure owner();

		public Rectangle displayBox();

		public boolean containsPoint(int x, int y);

		public void draw(Graphics g);

		public Undoable getUndoActivity();

		public void setUndoActivity(Undoable newUndoableActivity);

		public Cursor getCursor();


}
