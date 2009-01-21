
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.UndoableAdapter; 
import org.jhotdraw.util.Undoable; 
import java.awt.*; 
import java.awt.event.MouseEvent; 
public  class  DragTracker  extends AbstractTool {
		private Figure fAnchorFigure;

		private int fLastX, fLastY;

		private boolean fMoved = false;

		public DragTracker(DrawingEditor newDrawingEditor, Figure anchor) {	super(newDrawingEditor);	setAnchorFigure(anchor);	}

		public void mouseDown(MouseEvent e, int x, int y) {	super.mouseDown(e, x, y);	setLastMouseX(x);	setLastMouseY(y);	if (e.isShiftDown()) { getActiveView().toggleSelection(getAnchorFigure()); setAnchorFigure(null);	}	else if (!getActiveView().isFigureSelected(getAnchorFigure())) {	getActiveView().clearSelection();	getActiveView().addToSelection(getAnchorFigure());	}	setUndoActivity(createUndoActivity());	getUndoActivity().setAffectedFigures(getActiveView().selection());	}

		public void mouseDrag(MouseEvent e, int x, int y) {	super.mouseDrag(e, x, y);	setHasMoved((Math.abs(x - getAnchorX()) > 4) || (Math.abs(y - getAnchorY()) > 4));	if (hasMoved()) {	FigureEnumeration figures = getUndoActivity().getAffectedFigures();	while (figures.hasNextFigure()) {	figures.nextFigure().moveBy(x - getLastMouseX(), y - getLastMouseY());	}	}	setLastMouseX(x);	setLastMouseY(y);	}

		protected void setAnchorFigure(Figure newAnchorFigure) {	fAnchorFigure = newAnchorFigure;	}

		public Figure getAnchorFigure() {	return fAnchorFigure;	}

		protected void setLastMouseX(int newLastMouseX) {	fLastX = newLastMouseX;	}

		protected int getLastMouseX() {	return fLastX;	}

		protected void setLastMouseY(int newLastMouseY) {	fLastY = newLastMouseY;	}

		protected int getLastMouseY() {	return fLastY;	}

		public boolean hasMoved() {	return fMoved;	}

		protected void setHasMoved(boolean newMoved) {	fMoved = newMoved;	}

		public void activate() {	}

		public void deactivate() {	if (hasMoved()) {	((DragTracker.UndoActivity)getUndoActivity()).setBackupPoint(new Point(getLastMouseX(), getLastMouseY()));	}	else {	setUndoActivity(null);	}	}

		protected Undoable createUndoActivity() {	return new DragTracker.UndoActivity(getActiveView(), new Point(getLastMouseX(), getLastMouseY()));	}

		public static  class  UndoActivity  extends UndoableAdapter {
			private Point myOriginalPoint;

			private Point myBackupPoint;

			public UndoActivity(DrawingView newDrawingView, Point newOriginalPoint) {	super(newDrawingView);	setOriginalPoint(newOriginalPoint);	setUndoable(true);	setRedoable(true);	}

			public boolean undo() {	if (!super.undo()) {	return false;	}	moveAffectedFigures(getBackupPoint(), getOriginalPoint());	return true;	}

			public boolean redo() {	if (!super.redo()) {	return false;	}	moveAffectedFigures(getOriginalPoint(), getBackupPoint());	return true;	}

			public void setBackupPoint(Point newBackupPoint) {	myBackupPoint = newBackupPoint;	}

			public Point getBackupPoint() {	return myBackupPoint;	}

			public void setOriginalPoint(Point newOriginalPoint) {	myOriginalPoint = newOriginalPoint;	}

			public Point getOriginalPoint() {	return myOriginalPoint;	}

			public void moveAffectedFigures(Point startPoint, Point endPoint) {	FigureEnumeration figures = getAffectedFigures();	while (figures.hasNextFigure()) {	figures.nextFigure().moveBy(endPoint.x - startPoint.x,	endPoint.y - startPoint.y);	}	}


	}


}
