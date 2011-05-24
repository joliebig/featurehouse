
package org.jhotdraw.contrib; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.figures.PolyLineHandle; 
import org.jhotdraw.util.Undoable; 
import java.awt.Point; 
public  class  PolygonHandle  extends AbstractHandle {
		private Locator fLocator;

		private int fIndex;

		public PolygonHandle(PolygonFigure owner, Locator l, int index) {	super(owner);	fLocator = l;	fIndex = index;	}

		public void invokeStart(int x, int y, DrawingView view) {	setUndoActivity(createUndoActivity(view, fIndex));	getUndoActivity().setAffectedFigures(new SingleFigureEnumerator(owner()));	((PolygonHandle.UndoActivity)getUndoActivity()).setOldPoint(new Point(x, y));	}

		public void invokeStep(int x, int y, int anchorX, int anchorY, DrawingView view) {	int index = ((PolyLineHandle.UndoActivity)getUndoActivity()).getPointIndex();	myOwner().setPointAt(new Point(x, y), index);	}

		public void invokeEnd(int x, int y, int anchorX, int anchorY, DrawingView view) {	myOwner().smoothPoints();	if ((x == anchorX) && (y == anchorY)) {	setUndoActivity(null);	}	}

		public Point locate() {	return fLocator.locate(owner());	}

		private PolygonFigure myOwner() {	return (PolygonFigure)owner();	}

		protected Undoable createUndoActivity(DrawingView newView, int newPointIndex) {	return new PolygonHandle.UndoActivity(newView, newPointIndex);	}

		public static  class  UndoActivity  extends PolyLineHandle.UndoActivity {
			public UndoActivity(DrawingView newView, int newPointIndex) {	super(newView, newPointIndex);	}

			protected boolean movePointToOldLocation() {	FigureEnumeration fe = getAffectedFigures();	if (!fe.hasNextFigure()) {	return false;	}	PolygonFigure figure = (PolygonFigure)fe.nextFigure();	Point backupPoint = figure.pointAt(getPointIndex());	figure.setPointAt(getOldPoint(), getPointIndex());	figure.smoothPoints();	setOldPoint(backupPoint);	return true;	}


	}


}
