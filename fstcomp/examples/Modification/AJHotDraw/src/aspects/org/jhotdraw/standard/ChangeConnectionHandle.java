
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.Geom; 
import org.jhotdraw.util.Undoable; 
import org.jhotdraw.util.UndoableAdapter; 
import java.awt.*; 
public abstract  class  ChangeConnectionHandle  extends AbstractHandle {
		private Connector fOriginalTarget;

		private Figure myTarget;

		private ConnectionFigure myConnection;

		private Point fStart;

		protected ChangeConnectionHandle(ConnectionFigure owner) {	super(owner);	setConnection(owner);	setTargetFigure(null);	}

		protected abstract Connector target();

		protected abstract void disconnect();

		protected abstract void connect(Connector c);

		protected abstract void setPoint(int x, int y);

		protected Connector source() {	if (target() == getConnection().getStartConnector()) {	return getConnection().getEndConnector();	}	return getConnection().getStartConnector();	}

		public void invokeStart(int x, int y, DrawingView view) {	fOriginalTarget = target();	fStart = new Point(x, y);	setUndoActivity(createUndoActivity(view));	((ChangeConnectionHandle.UndoActivity)getUndoActivity()).setOldConnector(target());	disconnect();	}

		public void invokeStep (int x, int y, int anchorX, int anchorY, DrawingView view) {	Point p = new Point(x, y);	Figure f = findConnectableFigure(x, y, view.drawing());	if (f != getTargetFigure()) {	if (getTargetFigure() != null) {	getTargetFigure().connectorVisibility(false, null);	}	setTargetFigure(f);	if (getTargetFigure() != null) {	getTargetFigure().connectorVisibility(true, getConnection());	}	}	Connector target = findConnectionTarget(p.x, p.y, view.drawing());	if (target != null) {	p = Geom.center(target.displayBox());	}	setPoint(p.x, p.y);	}

		public void invokeEnd(int x, int y, int anchorX, int anchorY, DrawingView view) {	Connector target = findConnectionTarget(x, y, view.drawing());	if (target == null) {	target = fOriginalTarget;	}	setPoint(x, y);	connect(target);	getConnection().updateConnection();	Connector oldConnector = ((ChangeConnectionHandle.UndoActivity)	getUndoActivity()).getOldConnector();	if ((oldConnector == null)	|| (target() == null)	|| (oldConnector.owner() == target().owner())) {	setUndoActivity(null);	}	else {	getUndoActivity().setAffectedFigures(new SingleFigureEnumerator(getConnection()));	}	if (getTargetFigure() != null) {	getTargetFigure().connectorVisibility(false, null);	setTargetFigure(null);	}	}

		private Connector findConnectionTarget(int x, int y, Drawing drawing) {	Figure target = findConnectableFigure(x, y, drawing);	if ((target != null)	&& target.canConnect() && target != fOriginalTarget && !target.includes(owner())	&& canConnectTo(target)) {	return findConnector(x, y, target);	}	return null;	}

	 protected abstract boolean canConnectTo(Figure figure);

		protected Connector findConnector(int x, int y, Figure f) {	return f.connectorAt(x, y);	}

		public void draw(Graphics g) {	Rectangle r = displayBox();	g.setColor(Color.green);	g.fillRect(r.x, r.y, r.width, r.height);	g.setColor(Color.black);	g.drawRect(r.x, r.y, r.width, r.height);	}

		private Figure findConnectableFigure(int x, int y, Drawing drawing) {	FigureEnumeration fe = drawing.figuresReverse();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	if (!figure.includes(getConnection()) && figure.canConnect()) {	if (figure.containsPoint(x, y)) {	return figure;	}	}	}	return null;	}

		protected void setConnection(ConnectionFigure newConnection) {	myConnection = newConnection;	}

		protected ConnectionFigure getConnection() {	return myConnection;	}

		protected void setTargetFigure(Figure newTarget) {	myTarget = newTarget;	}

		protected Figure getTargetFigure() {	return myTarget;	}

		protected abstract Undoable createUndoActivity(DrawingView newView);

		public static abstract  class  UndoActivity  extends UndoableAdapter {
			private Connector myOldConnector;

			public UndoActivity(DrawingView newView) {	super(newView);	setUndoable(true);	setRedoable(true);	}

			public boolean undo() {	if (!super.undo()) {	return false;	}	swapConnectors();	return true;	}

			public boolean redo() {	if (!isRedoable()) {	return false;	}	swapConnectors();	return true;	}

			private void swapConnectors() {	FigureEnumeration fe = getAffectedFigures();	if (fe.hasNextFigure()) {	ConnectionFigure connection = (ConnectionFigure)fe.nextFigure();	setOldConnector(replaceConnector(connection));	connection.updateConnection();	}	}

			protected abstract Connector replaceConnector(ConnectionFigure connection);

			public void setOldConnector(Connector newOldConnector) {	myOldConnector = newOldConnector;	}

			public Connector getOldConnector() {	return myOldConnector;	}


	}


}
