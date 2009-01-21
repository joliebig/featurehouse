
package org.jhotdraw.util; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.AbstractTool; 
import java.awt.event.MouseEvent; 
import java.awt.event.KeyEvent; 
import java.util.EventObject; 
public  class  UndoableTool  implements Tool, ToolListener {
		private Tool myWrappedTool;

		private AbstractTool.EventDispatcher myEventDispatcher;

		public UndoableTool(Tool newWrappedTool) {	setEventDispatcher(createEventDispatcher());	setWrappedTool(newWrappedTool);	getWrappedTool().addToolListener(this);	}

		public void activate() {	getWrappedTool().activate();	}

		public void deactivate() {	getWrappedTool().deactivate();	Undoable undoActivity = getWrappedTool().getUndoActivity();	if ((undoActivity != null) && (undoActivity.isUndoable())) {	editor().getUndoManager().pushUndo(undoActivity);	editor().getUndoManager().clearRedos();	editor().figureSelectionChanged(getActiveView());	}	}

		public void mouseDown(MouseEvent e, int x, int y) {	getWrappedTool().mouseDown(e, x, y);	}

		public void mouseDrag(MouseEvent e, int x, int y) {	getWrappedTool().mouseDrag(e, x, y);	}

		public void mouseUp(MouseEvent e, int x, int y) {	getWrappedTool().mouseUp(e, x, y);	}

		public void mouseMove(MouseEvent evt, int x, int y) {	getWrappedTool().mouseMove(evt, x, y);	}

		public void keyDown(KeyEvent evt, int key) {	getWrappedTool().keyDown(evt, key);	}

		public boolean isUsable() {	return getWrappedTool().isUsable();	}

		public boolean isActive() {	return editor().tool() == this;	}

		public boolean isEnabled() {	return getWrappedTool().isEnabled();	}

		public void setUsable(boolean newIsUsable) {	getWrappedTool().setUsable(newIsUsable);	}

		public void setEnabled(boolean newIsEnabled) {	getWrappedTool().setEnabled(newIsEnabled);	}

		protected void setWrappedTool(Tool newWrappedTool) {	myWrappedTool = newWrappedTool;	}

		protected Tool getWrappedTool() {	return myWrappedTool;	}

		public DrawingEditor editor() {	return getWrappedTool().editor();	}

		public DrawingView view() {	return editor().view();	}

		public Undoable getUndoActivity() {	return new UndoableAdapter(view());	}

		public void setUndoActivity(Undoable newUndoableActivity) {	}

		public void toolUsable(EventObject toolEvent) {	getEventDispatcher().fireToolUsableEvent();	}

		public void toolUnusable(EventObject toolEvent) {	getEventDispatcher().fireToolUnusableEvent();	}

		public void toolActivated(EventObject toolEvent) {	getEventDispatcher().fireToolActivatedEvent();	}

		public void toolDeactivated(EventObject toolEvent) {	getEventDispatcher().fireToolDeactivatedEvent();	}

		public void toolEnabled(EventObject toolEvent) {	getEventDispatcher().fireToolEnabledEvent();	}

		public void toolDisabled(EventObject toolEvent) {	getEventDispatcher().fireToolDisabledEvent();	}

		public void addToolListener(ToolListener newToolListener) {	getEventDispatcher().addToolListener(newToolListener);	}

		public void removeToolListener(ToolListener oldToolListener) {	getEventDispatcher().removeToolListener(oldToolListener);	}

		private void setEventDispatcher(AbstractTool.EventDispatcher newEventDispatcher) {	myEventDispatcher = newEventDispatcher;	}

		protected AbstractTool.EventDispatcher getEventDispatcher() {	return myEventDispatcher;	}

		public AbstractTool.EventDispatcher createEventDispatcher() {	return new AbstractTool.EventDispatcher(this);	}

		public DrawingView getActiveView() {	return editor().view();	}


}
