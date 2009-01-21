
package org.jhotdraw.figures; 
import java.awt.event.MouseEvent; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.standard.AbstractTool; 
import org.jhotdraw.standard.SingleFigureEnumerator; 
public  class  ScribbleTool  extends AbstractTool {
		private PolyLineFigure fScribble;

		private int fLastX, fLastY;

		private Figure myAddedFigure;

		public ScribbleTool(DrawingEditor newDrawingEditor) {	super(newDrawingEditor);	}

		public void activate() {	super.activate();	}

		public void deactivate() {	super.deactivate();	if (fScribble != null) {	if (fScribble.size().width < 4 || fScribble.size().height < 4) {	getActiveDrawing().remove(fScribble);	setUndoActivity(null);	}	fScribble = null;	}	}

		private void point(int x, int y) {	if (fScribble == null) {	fScribble = new PolyLineFigure(x, y);	setAddedFigure(view().add(fScribble));	}	else if (fLastX != x || fLastY != y) {	fScribble.addPoint(x, y);	}	fLastX = x;	fLastY = y;	}

		public void mouseDown(MouseEvent e, int x, int y) {	super.mouseDown(e,x,y);	if (e.getClickCount() >= 2) {	setUndoActivity(createUndoActivity());	getUndoActivity().setAffectedFigures(new SingleFigureEnumerator(getAddedFigure()));	}	else {	point(e.getX(), e.getY());	}	}

		public void mouseDrag(MouseEvent e, int x, int y) {	if (fScribble != null) {	point(e.getX(), e.getY());	}	}

		public void mouseUp(MouseEvent e, int x, int y) {	super.mouseUp(e, x, y);	if (e.getClickCount() >= 2) {	editor().toolDone();	}	}

		protected Figure getAddedFigure() {	return myAddedFigure;	}

		private void setAddedFigure(Figure newAddedFigure) {	myAddedFigure = newAddedFigure;	}


}
