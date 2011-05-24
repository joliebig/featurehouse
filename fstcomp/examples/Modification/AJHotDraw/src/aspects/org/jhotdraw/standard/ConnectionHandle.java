
package org.jhotdraw.standard; 
import java.awt.Color; 
import java.awt.Graphics; 
import java.awt.Point; 
import java.awt.Rectangle; 
import java.util.Vector; 
import org.jhotdraw.framework.ConnectionFigure; 
import org.jhotdraw.framework.Connector; 
import org.jhotdraw.framework.Cursor; 
import org.jhotdraw.framework.Drawing; 
import org.jhotdraw.framework.DrawingView; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.FigureEnumeration; 
import org.jhotdraw.framework.Locator; 
import org.jhotdraw.util.Geom; 
public  class  ConnectionHandle  extends LocatorHandle {
		private ConnectionFigure myConnection;

		private ConnectionFigure fPrototype;

		private Figure myTargetFigure;

		public ConnectionHandle(Figure owner, Locator l, ConnectionFigure prototype) {	super(owner, l);	fPrototype = prototype;	}

		public void invokeStart(int x, int y, DrawingView view) {	setConnection(createConnection());	setUndoActivity(createUndoActivity(view));	Vector v = new Vector();	v.add(getConnection());	getUndoActivity().setAffectedFigures(new FigureEnumerator(v));	Point p = locate();	getConnection().startPoint(p.x, p.y);	getConnection().endPoint(p.x, p.y);	view.drawing().add(getConnection());	}

		public void invokeStep (int x, int y, int anchorX, int anchorY, DrawingView view) {	Point p = new Point(x,y);	Figure f = findConnectableFigure(x, y, view.drawing());	if (f != getTargetFigure()) {	if (getTargetFigure() != null) {	getTargetFigure().connectorVisibility(false, null);	}	setTargetFigure(f);	if (getTargetFigure() != null) {	getTargetFigure().connectorVisibility(true, getConnection());	}	}	Connector target = findConnectionTarget(p.x, p.y, view.drawing());	if (target != null) {	p = Geom.center(target.displayBox());	}	getConnection().endPoint(p.x, p.y);	}

		public void invokeEnd(int x, int y, int anchorX, int anchorY, DrawingView view) {	Connector target = findConnectionTarget(x, y, view.drawing());	if (target != null) {	getConnection().connectStart(startConnector());	getConnection().connectEnd(target);	getConnection().updateConnection();	}	else {	view.drawing().remove(getConnection());	setUndoActivity(null);	}	setConnection(null);	if (getTargetFigure() != null) {	getTargetFigure().connectorVisibility(false, null);	setTargetFigure(null);	}	}

		private Connector startConnector() {	Point p = locate();	return owner().connectorAt(p.x, p.y);	}

		protected ConnectionFigure createConnection() {	return (ConnectionFigure)fPrototype.clone();	}

		protected Connector findConnectionTarget(int x, int y, Drawing drawing) {	Figure target = findConnectableFigure(x, y, drawing);	if ((target != null) && target.canConnect() && !target.includes(owner()) && getConnection().canConnect(owner(), target)) {	return findConnector(x, y, target);	}	return null;	}

		private Figure findConnectableFigure(int x, int y, Drawing drawing) {	FigureEnumeration fe = drawing.figuresReverse();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	if (!figure.includes(getConnection()) && figure.canConnect()	&& figure.containsPoint(x, y)) {	return figure;	}	}	return null;	}

		protected Connector findConnector(int x, int y, Figure f) {	return f.connectorAt(x, y);	}

		public void draw(Graphics g) {	Rectangle r = displayBox();	g.setColor(Color.blue);	g.drawOval(r.x, r.y, r.width, r.height);	}

		protected void setConnection(ConnectionFigure newConnection) {	myConnection = newConnection;	}

		protected ConnectionFigure getConnection() {	return myConnection;	}

		protected Figure getTargetFigure() {	return myTargetFigure;	}

		protected void setTargetFigure(Figure newTargetFigure) {	myTargetFigure = newTargetFigure;	}

		public Cursor getCursor() {	return new AWTCursor(java.awt.Cursor.HAND_CURSOR);	}


}
