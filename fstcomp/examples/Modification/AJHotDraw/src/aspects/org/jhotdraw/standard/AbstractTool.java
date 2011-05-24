
package org.jhotdraw.standard; 
import java.awt.event.KeyEvent; 
import java.awt.event.MouseEvent; 
import java.util.EventObject; 
import java.util.Iterator; 
import java.util.List; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.CollectionsFactory; 
import org.jhotdraw.util.Undoable; 
public abstract  class  AbstractTool  implements Tool {
		private DrawingEditor myDrawingEditor;

	 private int myAnchorX;

	 private int myAnchorY;

		private DrawingView myDrawingView;

		private Undoable myUndoActivity;

		private AbstractTool.EventDispatcher myEventDispatcher;

		private boolean myIsUsable;

		private boolean myIsEnabled;

		public AbstractTool(DrawingEditor newDrawingEditor) {	setEditor(newDrawingEditor);	setEventDispatcher(createEventDispatcher());	setEnabled(true);	checkUsable();	editor().addViewChangeListener(createViewChangeListener());	}

		public void activate() {	if (getActiveView() != null) {	getActiveView().clearSelection();	getActiveView().checkDamage();	getEventDispatcher().fireToolActivatedEvent();	}	}

		public void deactivate() {	if (isActive()) {	if (getActiveView() != null) {	getActiveView().setCursor(new AWTCursor(java.awt.Cursor.DEFAULT_CURSOR));	}	getEventDispatcher().fireToolDeactivatedEvent();	}	}

		protected void viewSelectionChanged(DrawingView oldView, DrawingView newView) {	if (isActive()) {	deactivate();	activate();	}	checkUsable();	}

		protected void viewCreated(DrawingView view) {	}

		protected void viewDestroying(DrawingView view) {	}

		public void mouseDown(MouseEvent e, int x, int y) { setAnchorX(x); setAnchorY(y);	setView((DrawingView)e.getSource());	}

		public void mouseDrag(MouseEvent e, int x, int y) {	}

		public void mouseUp(MouseEvent e, int x, int y) {	}

		public void mouseMove(MouseEvent evt, int x, int y) {	}

		public void keyDown(KeyEvent evt, int key) {	}

		public Drawing drawing() {	return view().drawing();	}

		public Drawing getActiveDrawing() { return getActiveView().drawing();	}

		public DrawingEditor editor() {	return myDrawingEditor;	}

		protected void setEditor(DrawingEditor newDrawingEditor) {	myDrawingEditor = newDrawingEditor;	}

		public DrawingView view() {	return myDrawingView;	}

		protected void setView(DrawingView newDrawingView) {	myDrawingView = newDrawingView;	}

		public DrawingView getActiveView() {	return editor().view();	}

		public boolean isUsable() {	return isEnabled() && myIsUsable;	}

		public void setUsable(boolean newIsUsable) {	if (isUsable() != newIsUsable) {	myIsUsable = newIsUsable;	if (isUsable()) {	getEventDispatcher().fireToolUsableEvent();	}	else {	getEventDispatcher().fireToolUnusableEvent();	}	}	}

		public void setEnabled(boolean newIsEnabled) {	if (isEnabled() != newIsEnabled) {	myIsEnabled = newIsEnabled;	if (isEnabled()) {	getEventDispatcher().fireToolEnabledEvent();	}	else {	getEventDispatcher().fireToolDisabledEvent();	setUsable(false);	deactivate();	}	}	}

		public boolean isEnabled() {	return myIsEnabled;	}

		protected void setAnchorX(int newAnchorX) {	myAnchorX = newAnchorX;	}

		protected int getAnchorX() {	return myAnchorX;	}

		protected void setAnchorY(int newAnchorY) {	myAnchorY = newAnchorY;	}

		protected int getAnchorY() {	return myAnchorY;	}

		public Undoable getUndoActivity() {	return myUndoActivity;	}

		public void setUndoActivity(Undoable newUndoActivity) {	myUndoActivity = newUndoActivity;	}

		public boolean isActive() {	return (editor().tool() == this) && isUsable();	}

		public void addToolListener(ToolListener newToolListener) {	getEventDispatcher().addToolListener(newToolListener);	}

		public void removeToolListener(ToolListener oldToolListener) {	getEventDispatcher().removeToolListener(oldToolListener);	}

		private void setEventDispatcher(AbstractTool.EventDispatcher newEventDispatcher) {	myEventDispatcher = newEventDispatcher;	}

		protected AbstractTool.EventDispatcher getEventDispatcher() {	return myEventDispatcher;	}

		protected AbstractTool.EventDispatcher createEventDispatcher() {	return new AbstractTool.EventDispatcher(this);	}

	 protected ViewChangeListener createViewChangeListener() { return new ViewChangeListener() { public void viewSelectionChanged(DrawingView oldView, DrawingView newView){	AbstractTool.this.viewSelectionChanged(oldView, newView); }	public void viewCreated(DrawingView view){	AbstractTool.this.viewCreated(view); }	public void viewDestroying(DrawingView view){	AbstractTool.this.viewDestroying(view); } }; }

		protected void checkUsable() {	if (isEnabled()) {	setUsable((getActiveView() != null) && getActiveView().isInteractive());	}	}

		public static  class  EventDispatcher {
			private List myRegisteredListeners;

			private Tool myObservedTool;

			public EventDispatcher(Tool newObservedTool) {	myRegisteredListeners = CollectionsFactory.current().createList();	myObservedTool = newObservedTool;	}

			public void fireToolUsableEvent() {	Iterator iter = myRegisteredListeners.iterator();	while (iter.hasNext()) {	((ToolListener)iter.next()).toolUsable(new EventObject(myObservedTool));	}	}

			public void fireToolUnusableEvent() {	Iterator iter = myRegisteredListeners.iterator();	while (iter.hasNext()) {	((ToolListener)iter.next()).toolUnusable(new EventObject(myObservedTool));	}	}

			public void fireToolActivatedEvent() {	Iterator iter = myRegisteredListeners.iterator();	while (iter.hasNext()) {	((ToolListener)iter.next()).toolActivated(new EventObject(myObservedTool));	}	}

			public void fireToolDeactivatedEvent() {	Iterator iter = myRegisteredListeners.iterator();	while (iter.hasNext()) {	((ToolListener)iter.next()).toolDeactivated(new EventObject(myObservedTool));	}	}

			public void fireToolEnabledEvent() {	Iterator iter = myRegisteredListeners.iterator();	while (iter.hasNext()) {	((ToolListener)iter.next()).toolEnabled(new EventObject(myObservedTool));	}	}

			public void fireToolDisabledEvent() {	Iterator iter = myRegisteredListeners.iterator();	while (iter.hasNext()) {	((ToolListener)iter.next()).toolDisabled(new EventObject(myObservedTool));	}	}

			public void addToolListener(ToolListener newToolListener) {	if (!myRegisteredListeners.contains(newToolListener)) {	myRegisteredListeners.add(newToolListener);	}	}

			public void removeToolListener(ToolListener oldToolListener) {	if (myRegisteredListeners.contains(oldToolListener)) {	myRegisteredListeners.remove(oldToolListener);	}	}


	}


}
