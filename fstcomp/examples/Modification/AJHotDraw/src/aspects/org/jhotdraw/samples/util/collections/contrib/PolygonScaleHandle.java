
package org.jhotdraw.contrib; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.Geom; 
import org.jhotdraw.util.Undoable; 
import org.jhotdraw.util.UndoableAdapter; 
import java.awt.*; 
 
class  PolygonScaleHandle  extends AbstractHandle {
		private Point fCurrent;

		public PolygonScaleHandle(PolygonFigure owner) {	super(owner);	}

		public void invokeStart(int x, int y, DrawingView view) {	fCurrent = new Point(x, y);	PolygonScaleHandle.UndoActivity activity = (PolygonScaleHandle.UndoActivity)createUndoActivity(view);	setUndoActivity(activity);	activity.setAffectedFigures(new SingleFigureEnumerator(owner()));	activity.setPolygon(((PolygonFigure)(owner())).getPolygon());	}

		public void invokeStep (int x, int y, int anchorX, int anchorY, DrawingView view) {	fCurrent = new Point(x, y);	Polygon polygon = ((PolygonScaleHandle.UndoActivity)getUndoActivity()).getPolygon();	((PolygonFigure)(owner())).scaleRotate(new Point(anchorX, anchorY), polygon, fCurrent);	}

		public void invokeEnd(int x, int y, int anchorX, int anchorY, DrawingView view) {	((PolygonFigure)(owner())).smoothPoints();	if ((fCurrent.x == anchorX) && (fCurrent.y == anchorY)) {	setUndoActivity(null);	}	fCurrent = null;	}

		public Point locate() {	if (fCurrent == null) {	return getOrigin();	}	else {	return fCurrent;	}	}

		Point getOrigin() {	Point outer = ((PolygonFigure)(owner())).outermostPoint();	Point ctr = ((PolygonFigure)(owner())).center();	double len = Geom.length(outer.x, outer.y, ctr.x, ctr.y);	if (len == 0) {	return new Point(outer.x - HANDLESIZE/2, outer.y + HANDLESIZE/2);	}	double u = HANDLESIZE / len;	if (u > 1.0) {	return new Point((outer.x * 3 + ctr.x)/4, (outer.y * 3 + ctr.y)/4);	}	else {	return new Point((int)(outer.x * (1.0 - u) + ctr.x * u),	(int)(outer.y * (1.0 - u) + ctr.y * u));	}	}

		public void draw(Graphics g) {	Rectangle r = displayBox();	g.setColor(Color.yellow);	g.fillOval(r.x, r.y, r.width, r.height);	g.setColor(Color.black);	g.drawOval(r.x, r.y, r.width, r.height);	}

		protected Undoable createUndoActivity(DrawingView newView) {	return new PolygonScaleHandle.UndoActivity(newView);	}

		public static  class  UndoActivity  extends UndoableAdapter {
			private Polygon myPolygon;

			public UndoActivity(DrawingView newView) {	super(newView);	setUndoable(true);	setRedoable(true);	}

			public boolean undo() {	if (!super.undo()) {	return false;	}	return resetPolygon();	}

			public boolean redo() {	if (!isRedoable()) {	return false;	}	return resetPolygon();	}

			protected boolean resetPolygon() {	FigureEnumeration fe = getAffectedFigures();	if (!fe.hasNextFigure()) {	return false;	}	PolygonFigure figure = (PolygonFigure)fe.nextFigure();	Polygon backupPolygon = figure.getPolygon();	figure.willChange();	figure.setInternalPolygon(getPolygon());	figure.changed();	setPolygon(backupPolygon);	return true;	}

			protected void setPolygon(Polygon newPolygon) {	myPolygon = newPolygon;	}

			public Polygon getPolygon() {	return myPolygon;	}


	}


}
