
package org.jhotdraw.util; 
import java.awt.Graphics; 
import java.awt.Point; 
import java.awt.Rectangle; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.framework.Drawing; 
import org.jhotdraw.framework.DrawingView; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.Handle; 
public  class  UndoableHandle  implements Handle {
		private Handle myWrappedHandle;

		private DrawingView myDrawingView;

		public UndoableHandle(Handle newWrappedHandle) {	setWrappedHandle(newWrappedHandle);	}

		public UndoableHandle(Handle newWrappedHandle, DrawingView newDrawingView) {	setWrappedHandle(newWrappedHandle);	setDrawingView(newDrawingView);	}

		public Point locate() {	return getWrappedHandle().locate();	}

		public void invokeStart(int x, int y, DrawingView view) {	getWrappedHandle().invokeStart(x, y, view);	}

		public void invokeStart(int x, int y, Drawing drawing) {	getWrappedHandle().invokeStart(x, y, drawing);	}

		public void invokeStep(int x, int y, int anchorX, int anchorY, DrawingView view) {	getWrappedHandle().invokeStep(x, y, anchorX, anchorY, view);	}

		public void invokeStep(int dx, int dy, Drawing drawing) {	getWrappedHandle().invokeStep(dx, dy, drawing);	}

		public void invokeEnd(int x, int y, int anchorX, int anchorY, DrawingView view) {	getWrappedHandle().invokeEnd(x, y, anchorX, anchorY, view);	Undoable undoableActivity = getWrappedHandle().getUndoActivity();	if ((undoableActivity != null) && (undoableActivity.isUndoable())) {	view.editor().getUndoManager().pushUndo(undoableActivity);	view.editor().getUndoManager().clearRedos();	}	}

		public void invokeEnd(int dx, int dy, Drawing drawing) {	getWrappedHandle().invokeEnd(dx, dy, drawing);	}

		public Figure owner() {	return getWrappedHandle().owner();	}

		public Rectangle displayBox() {	return getWrappedHandle().displayBox();	}

		public boolean containsPoint(int x, int y) {	return getWrappedHandle().containsPoint(x, y);	}

		public void draw(Graphics g) {	getWrappedHandle().draw(g);	}

		protected void setWrappedHandle(Handle newWrappedHandle) {	myWrappedHandle = newWrappedHandle;	}

		protected Handle getWrappedHandle() {	return myWrappedHandle;	}

		public DrawingView getDrawingView() {	return myDrawingView;	}

		protected void setDrawingView(DrawingView newDrawingView) {	myDrawingView = newDrawingView;	}

		public Undoable getUndoActivity() {	return new UndoableAdapter(getDrawingView());	}

		public void setUndoActivity(Undoable newUndoableActivity) {	}

		public Cursor getCursor() {	return getWrappedHandle().getCursor();	}


}
