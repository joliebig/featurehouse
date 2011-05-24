
package org.jhotdraw.contrib; 
import java.awt.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.*; 
import org.jhotdraw.standard.*; 
 
class  TriangleRotationHandle  extends AbstractHandle {
		private Point fOrigin;

		public TriangleRotationHandle(TriangleFigure owner) {	super(owner);	}

		public void invokeStart(int x, int y, DrawingView view) {	fOrigin = getOrigin();	TriangleRotationHandle.UndoActivity activity =	(TriangleRotationHandle.UndoActivity)createUndoActivity(view);	setUndoActivity(activity);	activity.setAffectedFigures(new SingleFigureEnumerator(owner()));	double rotation = ((TriangleFigure)(owner())).getRotationAngle();	activity.setRotationAngle(rotation);	}

		public void invokeStep (int x, int y, int anchorX, int anchorY, DrawingView view) {	Point fCenter = owner().center();	double angle = Math.atan2(fOrigin.y + y - anchorY - fCenter.y, fOrigin.x + x - anchorX - fCenter.x);	((TriangleFigure)(owner())).rotate(angle);	}

		public void invokeEnd(int x, int y, int anchorX, int anchorY, DrawingView view) {	fOrigin = null;	}

		public Point locate() {	return getOrigin();	}

		Point getOrigin() {	Polygon p = ((TriangleFigure)(owner())).getPolygon();	Point first = new Point(p.xpoints[0], p.ypoints[0]);	Point ctr = owner().center();	double len = Geom.length(first.x, first.y, ctr.x, ctr.y);	if (len == 0) {	return new Point(first.x - HANDLESIZE/2, first.y + HANDLESIZE/2);	}	double u = HANDLESIZE / len;	if (u > 1.0) {	return new Point((first.x * 3 + ctr.x)/4, (first.y * 3 + ctr.y)/4);	}	else {	return new Point((int)(first.x * (1.0 - u) + ctr.x * u),	(int)(first.y * (1.0 - u) + ctr.y * u));	}	}

		public void draw(Graphics g) {	Rectangle r = displayBox();	g.setColor(Color.yellow);	g.fillOval(r.x, r.y, r.width, r.height);	g.setColor(Color.black);	g.drawOval(r.x, r.y, r.width, r.height);	}

		protected Undoable createUndoActivity(DrawingView newView) {	return new TriangleRotationHandle.UndoActivity(newView);	}

		public static  class  UndoActivity  extends UndoableAdapter {
			private double myRotationAngle;

			public UndoActivity(DrawingView newView) {	super(newView);	setUndoable(true);	setRedoable(true);	}

			public boolean undo() {	if (!super.undo()) {	return false;	}	return resetRotationAngle();	}

			public boolean redo() {	if (!isRedoable()) {	return false;	}	return resetRotationAngle();	}

			protected boolean resetRotationAngle() {	FigureEnumeration fe = getAffectedFigures();	if (!fe.hasNextFigure()) {	return false;	}	TriangleFigure figure = (TriangleFigure)fe.nextFigure();	double backupAngle = figure.getRotationAngle();	figure.willChange();	figure.rotate(getRotationAngle());	figure.changed();	setRotationAngle(backupAngle);	return true;	}

			protected void setRotationAngle(double newRotationAngle) {	myRotationAngle = newRotationAngle;	}

			public double getRotationAngle() {	return myRotationAngle;	}


	}


}
