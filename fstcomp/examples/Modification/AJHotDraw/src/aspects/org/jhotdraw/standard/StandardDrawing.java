
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.CollectionsFactory; 
import java.awt.*; 
import java.util.*; 
import java.util.List; 
import java.io.*; 
public  class  StandardDrawing  extends CompositeFigure  implements Drawing {
		private transient List fListeners;

		private transient Thread fDrawingLockHolder = null;

		private String	myTitle;

		private static final long serialVersionUID = -2602151437447962046L;

		private int drawingSerializedDataVersion = 1;

		public StandardDrawing() {	super();	fListeners = CollectionsFactory.current().createList(2);	init(new Rectangle(-500, -500, 2000, 2000));	}

		public void addDrawingChangeListener(DrawingChangeListener listener) {	if (fListeners == null) {	fListeners = CollectionsFactory.current().createList(2);	}	fListeners.add(listener);	}

		public void removeDrawingChangeListener(DrawingChangeListener listener) {	fListeners.remove(listener);	}

		public Iterator drawingChangeListeners() {	return fListeners.iterator();	}

		public synchronized Figure orphan(Figure figure) {	Figure orphanedFigure = super.orphan(figure);	if (orphanedFigure.listener() != null) {	Rectangle rect = invalidateRectangle(displayBox());	orphanedFigure.listener().figureRequestRemove(new FigureChangeEvent(orphanedFigure, rect));	}	return orphanedFigure;	}

		public synchronized Figure add(Figure figure) {	Figure addedFigure = super.add(figure);	if (addedFigure.listener() != null) {	Rectangle rect = invalidateRectangle(displayBox());	addedFigure.listener().figureRequestUpdate(new FigureChangeEvent(figure, rect));	return addedFigure;	}	return addedFigure;	}

		public void figureInvalidated(FigureChangeEvent e) {	if (fListeners != null) {	for (int i = 0; i < fListeners.size(); i++) {	DrawingChangeListener l = (DrawingChangeListener)fListeners.get(i);	l.drawingInvalidated(new DrawingChangeEvent(this, e.getInvalidatedRectangle()));	}	}	}

		public void fireDrawingTitleChanged() {	if (fListeners != null) {	for (int i = 0; i < fListeners.size(); i++) {	DrawingChangeListener l = (DrawingChangeListener)fListeners.get(i);	l.drawingTitleChanged(new DrawingChangeEvent(this, null));	}	}	}

		public void figureRequestUpdate(FigureChangeEvent e) {	if (fListeners != null) {	for (int i = 0; i < fListeners.size(); i++) {	DrawingChangeListener l = (DrawingChangeListener)fListeners.get(i);	l.drawingRequestUpdate(new DrawingChangeEvent(this, null));	}	}	}

		public HandleEnumeration handles() {	List handles = CollectionsFactory.current().createList();	handles.add(new NullHandle(this, RelativeLocator.northWest()));	handles.add(new NullHandle(this, RelativeLocator.northEast()));	handles.add(new NullHandle(this, RelativeLocator.southWest()));	handles.add(new NullHandle(this, RelativeLocator.southEast()));	return new HandleEnumerator(handles);	}

		public Rectangle displayBox() {	if (fFigures.size() > 0) {	FigureEnumeration fe = figures();	Rectangle r = fe.nextFigure().displayBox();	while (fe.hasNextFigure()) {	r.add(fe.nextFigure().displayBox());	}	return r;	}	return new Rectangle(0, 0, 0, 0);	}

		public void basicDisplayBox(Point p1, Point p2) {	}

		public synchronized void lock() {	Thread current = Thread.currentThread();	if (fDrawingLockHolder == current) {	return;	}	while (fDrawingLockHolder != null) {	try {	wait();	}	catch (InterruptedException ex) { }	}	fDrawingLockHolder = current;	}

		public synchronized void unlock() {	if (fDrawingLockHolder != null) {	fDrawingLockHolder = null;	notify();	}	}

		private void readObject(ObjectInputStream s)	throws ClassNotFoundException, IOException {	s.defaultReadObject();	fListeners = CollectionsFactory.current().createList(2);	}

		public String getTitle() {	return myTitle;	}

		public void setTitle(String newTitle) {	myTitle = newTitle;	}


}
