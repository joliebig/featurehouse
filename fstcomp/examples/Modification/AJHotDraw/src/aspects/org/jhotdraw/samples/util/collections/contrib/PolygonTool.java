
package org.jhotdraw.contrib; 
import java.awt.Point; 
import java.awt.event.MouseEvent; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.standard.AbstractTool; 
import org.jhotdraw.standard.SingleFigureEnumerator; 
public  class  PolygonTool  extends AbstractTool {
		private PolygonFigure fPolygon;

		private int fLastX, fLastY;

		private Figure myAddedFigure;

		public PolygonTool(DrawingEditor newDrawingEditor) {	super(newDrawingEditor);	}

		public void activate() {	super.activate();	fPolygon = null;	}

		public void deactivate() {	if (fPolygon != null) {	fPolygon.smoothPoints();	if (fPolygon.pointCount() < 3 ||	fPolygon.size().width < 4 || fPolygon.size().height < 4) {	getActiveView().drawing().remove(fPolygon);	setUndoActivity(null);	}	}	fPolygon = null;	super.deactivate();	}

		private void addPoint(int x, int y) {	if (fPolygon == null) {	fPolygon = new PolygonFigure(x, y);	setAddedFigure(view().add(fPolygon));	fPolygon.addPoint(x, y);	}	else if (fLastX != x || fLastY != y) {	fPolygon.addPoint(x, y);	}	fLastX = x;	fLastY = y;	}

		public void mouseDown(MouseEvent e, int x, int y) {	super.mouseDown(e,x,y);	x = e.getX();	y = e.getY();	if (e.getClickCount() >= 2) {	if (fPolygon != null) {	fPolygon.smoothPoints();	setUndoActivity(createUndoActivity());	getUndoActivity().setAffectedFigures(new SingleFigureEnumerator(getAddedFigure()));	editor().toolDone();	}	fPolygon = null;	}	else {	addPoint(e.getX(), e.getY());	}	}

		public void mouseMove(MouseEvent e, int x, int y) {	if (e.getSource() == getActiveView()) {	if (fPolygon != null) {	if (fPolygon.pointCount() > 1) {	fPolygon.setPointAt(new Point(x, y), fPolygon.pointCount()-1);	getActiveView().checkDamage();	}	}	}	}

		public void mouseDrag(MouseEvent e, int x, int y) {	x = e.getX();	y = e.getY();	addPoint(x, y);	}

		public void mouseUp(MouseEvent e, int x, int y) {	}

		protected Figure getAddedFigure() {	return myAddedFigure;	}

		private void setAddedFigure(Figure newAddedFigure) {	myAddedFigure = newAddedFigure;	}


}
