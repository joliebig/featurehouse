
package org.jhotdraw.samples.net; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.figures.*; 
import org.jhotdraw.util.*; 
import java.util.*; 
import java.util.List; 
import java.awt.*; 
public  class  NodeFigure  extends TextFigure {
		private static final int BORDER = 6;

		private List fConnectors;

		private boolean fConnectorsVisible;

		public NodeFigure() {	initialize();	fConnectors = null;	}

		public Rectangle displayBox() {	Rectangle box = super.displayBox();	int d = BORDER;	box.grow(d, d);	return box;	}

		public boolean containsPoint(int x, int y) {	if (fConnectorsVisible) {	Rectangle r = displayBox();	int d = LocatorConnector.SIZE/2;	r.grow(d, d);	return r.contains(x, y);	}	return super.containsPoint(x, y);	}

		private void drawBorder(Graphics g) {	Rectangle r = displayBox();	g.setColor(getFrameColor());	g.drawRect(r.x, r.y, r.width-1, r.height-1);	}

		public void draw(Graphics g) {	super.draw(g);	drawBorder(g);	drawConnectors(g);	}

		public HandleEnumeration handles() {	ConnectionFigure prototype = new LineConnection();	List handles = CollectionsFactory.current().createList();	handles.add(new ConnectionHandle(this, RelativeLocator.east(), prototype));	handles.add(new ConnectionHandle(this, RelativeLocator.west(), prototype));	handles.add(new ConnectionHandle(this, RelativeLocator.south(), prototype));	handles.add(new ConnectionHandle(this, RelativeLocator.north(), prototype));	handles.add(new NullHandle(this, RelativeLocator.southEast()));	handles.add(new NullHandle(this, RelativeLocator.southWest()));	handles.add(new NullHandle(this, RelativeLocator.northEast()));	handles.add(new NullHandle(this, RelativeLocator.northWest()));	return new HandleEnumerator(handles);	}

		private void drawConnectors(Graphics g) {	if (fConnectorsVisible) {	Iterator iter = connectors();	while (iter.hasNext()) {	((Connector)iter.next()).draw(g);	}	}	}

		public void connectorVisibility(boolean isVisible, ConnectionFigure courtingConnection) {	fConnectorsVisible = isVisible;	invalidate();	}

		public Connector connectorAt(int x, int y) {	return findConnector(x, y);	}

		private Iterator connectors() {	if (fConnectors == null) {	createConnectors();	}	return fConnectors.iterator();	}

		private void createConnectors() {	fConnectors = CollectionsFactory.current().createList(4);	fConnectors.add(new LocatorConnector(this, RelativeLocator.north()) );	fConnectors.add(new LocatorConnector(this, RelativeLocator.south()) );	fConnectors.add(new LocatorConnector(this, RelativeLocator.west()) );	fConnectors.add(new LocatorConnector(this, RelativeLocator.east()) );	}

		private Connector findConnector(int x, int y) {	long min = Long.MAX_VALUE;	Connector closest = null;	Iterator iter = connectors();	while (iter.hasNext()) {	Connector c = (Connector)iter.next();	Point p2 = Geom.center(c.displayBox());	long d = Geom.length2(x, y, p2.x, p2.y);	if (d < min) {	min = d;	closest = c;	}	}	return closest;	}

		private void initialize() {	setText("node");	Font fb = new Font("Helvetica", Font.BOLD, 12);	setFont(fb);	createConnectors();	}

		public Figure getRepresentingFigure() {	return this;	}


}
