
package org.jhotdraw.figures; 
import java.awt.*; 
import java.util.List; 
import java.io.*; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.*; 
public  class  LineConnection  extends PolyLineFigure  implements ConnectionFigure {
		protected Connector myStartConnector;

		protected Connector myEndConnector;

		private static final long serialVersionUID = 6883731614578414801L;

		private int lineConnectionSerializedDataVersion = 1;

		public LineConnection() {	super(4);	setStartDecoration(new ArrowTip());	setEndDecoration(new ArrowTip());	}

		public boolean canConnect() {	return false;	}

		protected void basicMoveBy(int dx, int dy) {	for (int i = 1; i < fPoints.size()-1; i++) {	pointAt(i).translate(dx, dy);	}	updateConnection();	}

		public void connectStart(Connector newStartConnector) {	setStartConnector(newStartConnector);	if (newStartConnector != null) {	startFigure().addDependendFigure(this);	startFigure().addFigureChangeListener(this);	}	}

		public void connectEnd(Connector newEndConnector) {	setEndConnector(newEndConnector);	if (newEndConnector != null) {	endFigure().addDependendFigure(this);	endFigure().addFigureChangeListener(this);	handleConnect(startFigure(), endFigure());	}	}

		public void disconnectStart() {	startFigure().removeFigureChangeListener(this);	startFigure().removeDependendFigure(this);	setStartConnector(null);	}

		public void disconnectEnd() {	handleDisconnect(startFigure(), endFigure());	endFigure().removeFigureChangeListener(this);	endFigure().removeDependendFigure(this);	setEndConnector(null);	}

		public boolean connectsSame(ConnectionFigure other) {	return other.getStartConnector() == getStartConnector()	&& other.getEndConnector() == getEndConnector();	}

		protected void handleDisconnect(Figure start, Figure end) {}

		protected void handleConnect(Figure start, Figure end) {}

		public Figure startFigure() {	if (getStartConnector() != null) {	return getStartConnector().owner();	}	return null;	}

		public Figure endFigure() {	if (getEndConnector() != null) {	return getEndConnector().owner();	}	return null;	}

		protected void setStartConnector(Connector newStartConnector) {	myStartConnector = newStartConnector;	}

		public Connector getStartConnector() {	return myStartConnector;	}

		protected void setEndConnector(Connector newEndConnector) {	myEndConnector = newEndConnector;	}

		public Connector getEndConnector() {	return myEndConnector;	}

		public boolean canConnect(Figure start, Figure end) {	return true;	}

		public void startPoint(int x, int y) {	willChange();	if (fPoints.size() == 0) {	fPoints.add(new Point(x, y));	}	else {	fPoints.set(0, new Point(x, y));	}	changed();	}

		public void endPoint(int x, int y) {	willChange();	if (fPoints.size() < 2) {	fPoints.add(new Point(x, y));	}	else {	fPoints.set(fPoints.size()-1, new Point(x, y));	}	changed();	}

		public Point startPoint() {	Point p = pointAt(0);	return new Point(p.x, p.y);	}

		public Point endPoint() {	if (fPoints.size() > 0) {	Point p = pointAt(fPoints.size()-1);	return new Point(p.x, p.y);	}	else {	return null;	}	}

		public HandleEnumeration handles() {	List handles = CollectionsFactory.current().createList(fPoints.size());	handles.add(new ChangeConnectionStartHandle(this));	for (int i = 1; i < fPoints.size()-1; i++) {	handles.add(new PolyLineHandle(this, locator(i), i));	}	handles.add(new ChangeConnectionEndHandle(this));	return new HandleEnumerator(handles);	}

		public void setPointAt(Point p, int i) {	super.setPointAt(p, i);	layoutConnection();	}

		public void insertPointAt(Point p, int i) {	super.insertPointAt(p, i);	layoutConnection();	}

		public void removePointAt(int i) {	super.removePointAt(i);	layoutConnection();	}

		public void updateConnection() {	if (getStartConnector() != null) {	Point start = getStartConnector().findStart(this);	if (start != null) {	startPoint(start.x, start.y);	}	}	if (getEndConnector() != null) {	Point end = getEndConnector().findEnd(this);	if (end != null) {	endPoint(end.x, end.y);	}	}	}

		public void layoutConnection() {	updateConnection();	}

		public void figureChanged(FigureChangeEvent e) {	updateConnection();	}

		public void figureRemoved(FigureChangeEvent e) {	}

		public void figureRequestRemove(FigureChangeEvent e) {	}

		public void figureInvalidated(FigureChangeEvent e) {	}

		public void figureRequestUpdate(FigureChangeEvent e) {	}

		public void release() {	super.release();	handleDisconnect(startFigure(), endFigure());	if (getStartConnector() != null) {	startFigure().removeFigureChangeListener(this);	startFigure().removeDependendFigure(this);	}	if (getEndConnector() != null) {	endFigure().removeFigureChangeListener(this);	endFigure().removeDependendFigure(this);	}	}

		private void readObject(ObjectInputStream s)	throws ClassNotFoundException, IOException {	s.defaultReadObject();	if (getStartConnector() != null) {	connectStart(getStartConnector());	}	if (getEndConnector() != null) {	connectEnd(getEndConnector());	}	}

		public void visit(FigureVisitor visitor) {	visitor.visitFigure(this);	}

		public void removeFromContainer(FigureChangeListener c) {	super.removeFromContainer(c);	release();	}


}
