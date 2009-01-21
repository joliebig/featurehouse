
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.UndoableTool; 
import org.jhotdraw.util.UndoableHandle; 
import org.jhotdraw.contrib.dnd.DragNDropTool; 
import java.awt.event.MouseEvent; 
public  class  SelectionTool  extends AbstractTool {
		private Tool myDelegationTool = null;

		public SelectionTool(DrawingEditor newDrawingEditor) {	super(newDrawingEditor);	}

		public void mouseDown(MouseEvent e, int x, int y) {	super.mouseDown(e, x, y);	if (getDelegateTool() != null) {	return;	}	view().freezeView();	Handle handle = view().findHandle(e.getX(), e.getY());	if (handle != null) {	setDelegateTool(createHandleTracker(view(), handle));	}	else {	Figure figure = drawing().findFigure(e.getX(), e.getY());	if (figure != null) {	setDelegateTool(createDragTracker(figure));	}	else {	if (!e.isShiftDown()) {	view().clearSelection();	}	setDelegateTool(createAreaTracker());	}	}	getDelegateTool().activate();	getDelegateTool().mouseDown(e, x, y);	}

		public void mouseMove(MouseEvent evt, int x, int y) {	if (evt.getSource() == getActiveView() ) {	DragNDropTool.setCursor(evt.getX(), evt.getY(), getActiveView());	}	}

		public void mouseDrag(MouseEvent e, int x, int y) {	if (getDelegateTool() != null) {	getDelegateTool().mouseDrag(e, x, y);	}	}

		public void mouseUp(MouseEvent e, int x, int y) {	if (getDelegateTool() != null) {	getDelegateTool().mouseUp(e, x, y);	getDelegateTool().deactivate();	setDelegateTool(null);	}	if (view() != null) {	view().unfreezeView();	editor().figureSelectionChanged(view());	}	}

		protected Tool createHandleTracker(DrawingView view, Handle handle) {	return new HandleTracker(editor(), new UndoableHandle(handle));	}

		protected Tool createDragTracker(Figure f) {	return new UndoableTool(new DragTracker(editor(), f));	}

		protected Tool createAreaTracker() {	return new SelectAreaTracker(editor());	}

		protected Tool getDelegateTool() {	return myDelegationTool;	}

		protected final void setDelegateTool(Tool newDelegateTool) {	myDelegationTool = newDelegateTool;	}


}
