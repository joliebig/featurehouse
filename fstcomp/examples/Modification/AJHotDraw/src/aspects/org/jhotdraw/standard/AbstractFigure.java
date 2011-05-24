
package org.jhotdraw.standard; 
import org.jhotdraw.util.*; 
import org.jhotdraw.framework.*; 
import java.awt.*; 
import java.util.List; 
import java.io.*; 
public abstract  class  AbstractFigure  implements Figure {
		private transient FigureChangeListener fListener;

		private List myDependendFigures;

		private static final long serialVersionUID = -10857585979273442L;

		private int abstractFigureSerializedDataVersion = 1;

		private int _nZ;

		protected AbstractFigure() { myDependendFigures = CollectionsFactory.current().createList();	}

		public void moveBy(int dx, int dy) {	willChange();	basicMoveBy(dx, dy);	changed();	}

		protected abstract void basicMoveBy(int dx, int dy);

		public void displayBox(Point origin, Point corner) {	willChange();	basicDisplayBox(origin, corner);	changed();	}

		public abstract void basicDisplayBox(Point origin, Point corner);

		public abstract Rectangle displayBox();

		public abstract HandleEnumeration handles();

		public FigureEnumeration figures() {	return FigureEnumerator.getEmptyEnumeration();	}

		public Dimension size() {	return new Dimension(displayBox().width, displayBox().height);	}

		public boolean isEmpty() {	return (size().width < 3) || (size().height < 3);	}

		public Figure findFigureInside(int x, int y) {	if (containsPoint(x, y)) {	return this;	}	return null;	}

		public boolean containsPoint(int x, int y) {	return displayBox().contains(x, y);	}

		public void displayBox(Rectangle r) {	displayBox(new Point(r.x, r.y), new Point(r.x+r.width, r.y+r.height));	}

		public boolean includes(Figure figure) {	return figure == this;	}

		public FigureEnumeration decompose() {	List figures = CollectionsFactory.current().createList(1);	figures.add(this);	return new FigureEnumerator(figures);	}

		public void addToContainer(FigureChangeListener c) {	addFigureChangeListener(c);	invalidate();	}

		public void removeFromContainer(FigureChangeListener c) {	invalidate();	removeFigureChangeListener(c);	}

		public synchronized void addFigureChangeListener(FigureChangeListener l) {	fListener = FigureChangeEventMulticaster.add(listener(), l);	}

		public synchronized void removeFigureChangeListener(FigureChangeListener l) {	fListener = FigureChangeEventMulticaster.remove(listener(), l);	}

		public synchronized FigureChangeListener listener() {	return fListener;	}

		public void release() {	if (listener() != null) {	listener().figureRemoved(new FigureChangeEvent(this));	}	}

		public void invalidate() {	if (listener() != null) {	Rectangle r = invalidateRectangle(displayBox());	listener().figureInvalidated(new FigureChangeEvent(this, r));	}	}

		protected Rectangle invalidateRectangle(Rectangle r) {	r.grow(Handle.HANDLESIZE, Handle.HANDLESIZE);	return r;	}

		public void willChange() {	invalidate();	}

		public void changed() {	invalidate();	if (listener() != null) {	listener().figureChanged(new FigureChangeEvent(this));	}	}

		public Point center() {	return Geom.center(displayBox());	}

		public boolean canConnect() {	return true;	}

		public Insets connectionInsets() {	return new Insets(0, 0, 0, 0);	}

		public Connector connectorAt(int x, int y) {	return new ChopBoxConnector(this);	}

		public void connectorVisibility(boolean isVisible, ConnectionFigure connector) {	}

		public Locator connectedTextLocator(Figure text) {	return RelativeLocator.center();	}

		public Object getAttribute(String name) {	return null;	}

		public Object getAttribute(FigureAttributeConstant attributeConstant) {	return null;	}

		public void setAttribute(String name, Object value) {	}

		public void setAttribute(FigureAttributeConstant attributeConstant, Object value) {	}

		public Object clone() {	Object clone = null;	ByteArrayOutputStream output = new ByteArrayOutputStream(200);	try {	ObjectOutput writer = new ObjectOutputStream(output);	writer.writeObject(this);	writer.close();	}	catch (IOException e) {	System.err.println("Class not found: " + e);	}	InputStream input = new ByteArrayInputStream(output.toByteArray());	try {	ObjectInput reader = new ObjectInputStream(input);	clone = reader.readObject();	}	catch (IOException e) {	System.err.println(e.toString());	}	catch (ClassNotFoundException e) {	System.err.println("Class not found: " + e);	}	return clone;	}

		public int getZValue() { return _nZ;	}

		public void setZValue(int z) { _nZ = z;	}

		public void visit(FigureVisitor visitor) {	FigureEnumeration fe = getDependendFigures();	visitor.visitFigure(this);	FigureEnumeration visitFigures = figures();	while (visitFigures.hasNextFigure()) {	visitFigures.nextFigure().visit(visitor);	}	HandleEnumeration visitHandles = handles();	while (visitHandles.hasNextHandle()) {	visitor.visitHandle(visitHandles.nextHandle());	}	while (fe.hasNextFigure()) {	fe.nextFigure().visit(visitor);	}	}

		public synchronized FigureEnumeration getDependendFigures() {	return new FigureEnumerator(myDependendFigures);	}

		public synchronized void addDependendFigure(Figure newDependendFigure) {	myDependendFigures.add(newDependendFigure);	}

		public synchronized void removeDependendFigure(Figure oldDependendFigure) {	myDependendFigures.remove(oldDependendFigure);	}

		public TextHolder getTextHolder() {	return null;	}

		public Figure getDecoratedFigure() {	return this;	}


}
