
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.Geom; 
import org.jhotdraw.util.UndoableAdapter; 
import org.jhotdraw.util.Undoable; 
import java.awt.*; 
import java.awt.event.MouseEvent; 
public  class  ConnectionTool  extends AbstractTool {
		private Connector myStartConnector;

		private Connector myEndConnector;

		private Connector myTargetConnector;

		private Figure myTarget;

		private ConnectionFigure myConnection;

		private int fSplitPoint;

		private ConnectionFigure fEditedConnection;

		private Figure myAddedFigure;

		private ConnectionFigure fPrototype;

		public ConnectionTool(DrawingEditor newDrawingEditor, ConnectionFigure newPrototype) {	super(newDrawingEditor);	fPrototype = newPrototype;	}

		public void mouseMove(MouseEvent e, int x, int y) {	trackConnectors(e, x, y);	}

		public void mouseDown(MouseEvent e, int x, int y) {	super.mouseDown(e,x,y);	int ex = e.getX();	int ey = e.getY();	ConnectionFigure connection = findConnection(ex, ey, drawing());	if (connection != null) {	if (!connection.joinSegments(ex, ey)) {	fSplitPoint = connection.splitSegment(ex, ey);	fEditedConnection = connection;	}	else {	fEditedConnection = null;	}	}	else {	setTargetFigure(findConnectionStart(ex, ey, drawing()));	if (getTargetFigure() != null) {	setStartConnector(findConnector(ex, ey, getTargetFigure()));	if (getStartConnector() != null) {	setConnection(createConnection());	getConnection().startPoint(ex, ey);	getConnection().endPoint(ex, ey);	setAddedFigure(view().add(getConnection()));	}	}	}	}

		public void mouseDrag(MouseEvent e, int x, int y) {	Point p = new Point(e.getX(), e.getY());	if (getConnection() != null) {	trackConnectors(e, x, y);	if (getTargetConnector() != null) {	p = Geom.center(getTargetConnector().displayBox());	}	getConnection().endPoint(p.x, p.y);	}	else if (fEditedConnection != null) {	Point pp = new Point(x, y);	fEditedConnection.setPointAt(pp, fSplitPoint);	}	}

		public void mouseUp(MouseEvent e, int x, int y) {	Figure c = null;	if (getStartConnector() != null) {	c = findTarget(e.getX(), e.getY(), drawing());	}	if (c != null) {	setEndConnector(findConnector(e.getX(), e.getY(), c));	if (getEndConnector() != null) {	getConnection().connectStart(getStartConnector());	getConnection().connectEnd(getEndConnector());	getConnection().updateConnection();	setUndoActivity(createUndoActivity());	getUndoActivity().setAffectedFigures(	new SingleFigureEnumerator(getAddedFigure()));	}	}	else if (getConnection() != null) {	view().remove(getConnection());	}	setConnection(null);	setStartConnector(null);	setEndConnector(null);	setAddedFigure(null);	editor().toolDone();	}

		public void deactivate() {	super.deactivate();	if (getTargetFigure() != null) {	getTargetFigure().connectorVisibility(false, null);	}	}

		protected ConnectionFigure createConnection() {	return (ConnectionFigure)fPrototype.clone();	}

		protected Figure findSource(int x, int y, Drawing drawing) {	return findConnectableFigure(x, y, drawing);	}

		protected Figure findTarget(int x, int y, Drawing drawing) {	Figure target = findConnectableFigure(x, y, drawing);	Figure start = getStartConnector().owner();	if (target != null && getConnection() != null && target.canConnect() && !target.includes(start) && getConnection().canConnect(start, target)) {	return target;	}	return null;	}

		protected ConnectionFigure findConnection(int x, int y, Drawing drawing) {	FigureEnumeration fe = drawing.figuresReverse();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	figure = figure.findFigureInside(x, y);	if (figure != null && (figure instanceof ConnectionFigure)) {	return (ConnectionFigure)figure;	}	}	return null;	}

		protected void setConnection(ConnectionFigure newConnection) {	myConnection = newConnection;	}

		protected ConnectionFigure getConnection() {	return myConnection;	}

		protected void trackConnectors(MouseEvent e, int x, int y) {	Figure c = null;	if (getStartConnector() == null) {	c = findSource(x, y, getActiveDrawing());	}	else {	c = findTarget(x, y, getActiveDrawing());	}	if (c != getTargetFigure()) {	if (getTargetFigure() != null) {	getTargetFigure().connectorVisibility(false, null);	}	setTargetFigure(c);	if (getTargetFigure() != null) {	getTargetFigure().connectorVisibility(true, getConnection());	}	}	Connector cc = null;	if (c != null) {	cc = findConnector(e.getX(), e.getY(), c);	}	if (cc != getTargetConnector()) {	setTargetConnector(cc);	}	getActiveView().checkDamage();	}

		protected Connector findConnector(int x, int y, Figure f) {	return f.connectorAt(x, y);	}

		protected Figure findConnectionStart(int x, int y, Drawing drawing) {	Figure target = findConnectableFigure(x, y, drawing);	if ((target != null) && target.canConnect()) {	return target;	}	return null;	}

		protected Figure findConnectableFigure(int x, int y, Drawing drawing) {	FigureEnumeration fe = drawing.figuresReverse();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	if (!figure.includes(getConnection()) && figure.canConnect()	&& figure.containsPoint(x, y)) {	return figure;	}	}	return null;	}

		protected void setStartConnector(Connector newStartConnector) {	myStartConnector = newStartConnector;	}

		protected Connector getStartConnector() {	return myStartConnector;	}

		protected void setEndConnector(Connector newEndConnector) {	myEndConnector = newEndConnector;	}

		protected Connector getEndConnector() {	return myEndConnector;	}

		protected void setTargetConnector(Connector newTargetConnector) {	myTargetConnector = newTargetConnector;	}

		protected Connector getTargetConnector() {	return myTargetConnector;	}

		protected void setTargetFigure(Figure newTarget) {	myTarget = newTarget;	}

		protected Figure getTargetFigure() {	return myTarget;	}

		protected Figure getAddedFigure() {	return myAddedFigure;	}

		protected void setAddedFigure(Figure newAddedFigure) {	myAddedFigure = newAddedFigure;	}

		protected Undoable createUndoActivity() {	return new ConnectionTool.UndoActivity(view(), getConnection());	}

		public static  class  UndoActivity  extends UndoableAdapter {
			private ConnectionFigure myConnection;

			private Connector myStartConnector;

			private Connector myEndConnector;

			public UndoActivity(DrawingView newDrawingView, ConnectionFigure newConnection) {	super(newDrawingView);	setConnection(newConnection);	myStartConnector = getConnection().getStartConnector();	myEndConnector = getConnection().getEndConnector(); setUndoable(true);	setRedoable(true);	}

			public boolean undo() {	if (!super.undo()) {	return false;	}	getConnection().disconnectStart();	getConnection().disconnectEnd();	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	getDrawingView().drawing().orphan(fe.nextFigure());	}	getDrawingView().clearSelection();	return true;	}

			public boolean redo() {	if (!super.redo()) {	return false;	}	getConnection().connectStart(myStartConnector);	getConnection().connectEnd(myEndConnector);	getConnection().updateConnection();	getDrawingView().insertFigures(getAffectedFigures(), 0, 0, false);	return true;	}

			protected void setConnection(ConnectionFigure newConnection) {	myConnection = newConnection;	}

			protected ConnectionFigure getConnection() {	return myConnection;	}


	}


}
