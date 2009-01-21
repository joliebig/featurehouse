
package org.jhotdraw.standard; 
import java.awt.Point; 
import java.awt.event.MouseEvent; 
import java.util.List; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.JHotDrawRuntimeException; 
import org.jhotdraw.util.CollectionsFactory; 
public  class  CreationTool  extends AbstractTool {
		private List fAddedFigures;

		private Figure fCreatedFigure;

		private Figure myAddedFigure;

		private Figure myPrototypeFigure;

		public CreationTool(DrawingEditor newDrawingEditor, Figure prototype) {	super(newDrawingEditor);	setPrototypeFigure(prototype);	}

		protected CreationTool(DrawingEditor newDrawingEditor) {	this(newDrawingEditor, null);	}

		public void activate() {	super.activate();	if (isUsable()) {	getActiveView().setCursor(new AWTCursor(java.awt.Cursor.CROSSHAIR_CURSOR));	}	setAddedFigures(CollectionsFactory.current().createList());	}

		public void deactivate() {	setCreatedFigure(null);	setAddedFigure(null);	setAddedFigures(null);	super.deactivate();	}

		public void mouseDown(MouseEvent e, int x, int y) {	super.mouseDown(e, x, y);	setCreatedFigure(createFigure());	setAddedFigure(getActiveView().add(getCreatedFigure()));	getAddedFigure().displayBox(new Point(getAnchorX(), getAnchorY()), new Point(getAnchorX(), getAnchorY()));	}

		protected Figure createFigure() {	if (getPrototypeFigure() == null) {	throw new JHotDrawRuntimeException("No protoype defined");	}	return (Figure)getPrototypeFigure().clone();	}

		public void mouseDrag(MouseEvent e, int x, int y) {	if (getAddedFigure() != null) {	getAddedFigure().displayBox(new Point(getAnchorX(), getAnchorY()), new Point(x, y));	}	}

		public void mouseUp(MouseEvent e, int x, int y) {	if (getAddedFigure() != null && !getCreatedFigure().isEmpty()) {	getAddedFigures().add(getAddedFigure());	}	else {	getActiveView().remove(getAddedFigure());	}	if (getAddedFigures().isEmpty()) {	setUndoActivity(null);	}	else {	setUndoActivity(createUndoActivity());	getUndoActivity().setAffectedFigures(new FigureEnumerator(getAddedFigures()));	}	editor().toolDone();	}

		protected void setPrototypeFigure(Figure newPrototypeFigure) {	myPrototypeFigure = newPrototypeFigure;	}

		protected Figure getPrototypeFigure() {	return myPrototypeFigure;	}

		protected List getAddedFigures() {	return fAddedFigures;	}

		protected void setAddedFigures(List newAddedFigures) {	fAddedFigures = newAddedFigures;	}

		protected Figure getCreatedFigure() {	return fCreatedFigure;	}

		protected void setCreatedFigure(Figure newCreatedFigure) {	fCreatedFigure = newCreatedFigure;	}

		protected Figure getAddedFigure() {	return myAddedFigure;	}

		protected void setAddedFigure(Figure newAddedFigure) {	myAddedFigure = newAddedFigure;	}


}
