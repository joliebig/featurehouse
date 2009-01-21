
package org.jhotdraw.contrib.dnd; 
import java.awt.Component; 
import java.awt.Point; 
import java.awt.dnd.DragGestureListener; 
import java.awt.event.MouseEvent; 
import javax.swing.JComponent; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
public  class  DragNDropTool  extends AbstractTool {
		private Tool fChild;

		private DragGestureListener dragGestureListener;

		private boolean dragOn;

		public DragNDropTool(DrawingEditor editor) {	super(editor);	setDragGestureListener(createDragGestureListener());	dragOn = false;	}

		protected void viewCreated(DrawingView view) {	super.viewCreated(view);	if (DNDInterface.class.isInstance(view)) {	DNDInterface dndi = (DNDInterface)view;	dndi.DNDInitialize( getDragGestureListener() );	}	}

		protected void viewDestroying(DrawingView view) {	if (DNDInterface.class.isInstance(view)) {	DNDInterface dndi = (DNDInterface)view;	dndi.DNDDeinitialize();	}	super.viewDestroying(view);	}

		public void activate() {	super.activate();	setDragOn(true);	}

		public void deactivate() {	setDragOn(false);	super.deactivate();	}

		public static void setCursor(int x, int y, DrawingView view) {	if (view == null) {	return;	}	Handle handle = view.findHandle(x, y);	Figure figure = view.drawing().findFigure(x, y);	if (handle != null) {	view.setCursor(handle.getCursor());	}	else if (figure != null) {	view.setCursor(new AWTCursor(java.awt.Cursor.MOVE_CURSOR));	}	else {	view.setCursor(new AWTCursor(java.awt.Cursor.DEFAULT_CURSOR));	}	}

		public void mouseMove(MouseEvent evt, int x, int y) {	if (evt.getSource() == getActiveView()) {	setCursor(x, y, getActiveView());	}	}

		public void mouseUp(MouseEvent e, int x, int y) {	if (fChild != null) {	fChild.mouseUp(e, x, y);	fChild = null;	}	setDragOn(true);	view().unfreezeView();	}

		public void mouseDown(MouseEvent e, int x, int y) {	super.mouseDown(e, x, y);	if (fChild != null) {	return;	}	view().freezeView();	Handle handle = view().findHandle(getAnchorX(), getAnchorY());	if (handle != null) {	setDragOn(false);	fChild = createHandleTracker(handle);	}	else {	Figure figure = drawing().findFigure(getAnchorX(), getAnchorY());	if (figure != null) {	fChild = null;	if (e.isShiftDown()) { view().toggleSelection(figure);	}	else if (!view().isFigureSelected(figure)) {	view().clearSelection();	view().addToSelection(figure);	}	}	else {	setDragOn(false);	if (!e.isShiftDown()) {	view().clearSelection();	}	fChild = createAreaTracker();	}	}	if (fChild != null) {	fChild.mouseDown(e, x, y);	}	}

		public void mouseDrag(MouseEvent e, int x, int y) {	if (fChild != null) {	fChild.mouseDrag(e, x, y);	}	}

		protected Tool createAreaTracker() {	return new SelectAreaTracker(editor());	}

		protected Tool createDragTracker(DrawingEditor editor, Figure f) {	return new DragTracker(editor, f);	}

		protected Tool createHandleTracker(Handle handle) {	return new HandleTracker(editor(), handle);	}

		private DragGestureListener getDragGestureListener(){	return dragGestureListener;	}

		private void setDragGestureListener(DragGestureListener dragGestureListener){	this.dragGestureListener = dragGestureListener;	}

		protected boolean isDragOn(){	return dragOn;	}

		protected void setDragOn(boolean isNewDragOn){	this.dragOn = isNewDragOn;	}

		private DragGestureListener createDragGestureListener() {	return new DragGestureListener() {	public void dragGestureRecognized(final java.awt.dnd.DragGestureEvent dge) {	Component c = dge.getComponent();	if (isDragOn() == false) {	return;	}	if (c instanceof DrawingView) {	boolean found = false;	DrawingView dv = (DrawingView)c;	FigureEnumeration selectedElements = dv.selection();	if (selectedElements.hasNextFigure() == false) {	return;	}	Point p = dge.getDragOrigin();	while (selectedElements.hasNextFigure()) {	Figure f = selectedElements.nextFigure();	if (f.containsPoint(p.x, p.y)) {	found = true;	break;	}	}	if (found == true) {	DNDFigures dndff = new DNDFigures(dv.selection(), p);	DNDFiguresTransferable trans = new DNDFiguresTransferable(dndff);	if (c instanceof JComponent) {	((JComponent)c).setAutoscrolls(false);	}	dge.getDragSource().startDrag(	dge,	null,	trans,	((DNDInterface)dv).getDragSourceListener());	}	}	}	};	}


}
