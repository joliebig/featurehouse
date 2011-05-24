
package org.jhotdraw.contrib.zoom; 
import org.jhotdraw.framework.Drawing; 
import org.jhotdraw.framework.DrawingChangeEvent; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.FigureEnumeration; 
import org.jhotdraw.standard.StandardDrawing; 
import org.jhotdraw.standard.StandardDrawingView; 
import org.jhotdraw.util.Geom; 
import javax.swing.JViewport; 
import java.awt.*; 
import java.awt.event.*; 
import java.awt.geom.AffineTransform; 
public  class  ZoomDrawingView  extends StandardDrawingView {
		private double scale = 1.0;

		private double zoomSpeed = 2.0;

		public ZoomDrawingView(DrawingEditor editor) {	this(editor, MINIMUM_WIDTH, MINIMUM_HEIGHT);	}

		public ZoomDrawingView(DrawingEditor editor, int width, int height) {	super(editor, width, height);	}

		public final double getScale() {	return scale;	}

		private void setScale(double newScale) {	Dimension oldSize = getUserSize();	scale = newScale;	setUserSize(oldSize.width, oldSize.height);	centralize(drawing());	forceRedraw();	}

		private void forceRedraw() {	drawingInvalidated(new DrawingChangeEvent	(drawing(), new Rectangle(getSize())));	repairDamage();	}

		public void setUserSize(int width, int height) {	setSize((int) (width * getScale()),	(int) (height * getScale()));	}

		public void setUserSize(Dimension d) {	setUserSize(d.width, d.height);	}

		public Dimension getSize() {	return super.getSize();	}

		public Dimension getViewportSize() {	return getParent().getSize();	}

		protected boolean hasZoomSupport() {	return getParent() instanceof JViewport;	}

		public void setOriginPosition(Point newOrigin) {	setViewPosition(newOrigin);	forceRedraw();	}

		protected void setViewPosition(Point newPosition) {	((JViewport)getParent()).setViewPosition(newPosition);	}

		public Dimension getUserSize() {	Dimension screenSize = getSize();	return new Dimension((int) (screenSize.width / getScale()),	(int) (screenSize.height / getScale()));	}

		public void zoom(int x, int y, int width, int height) {	if (hasZoomSupport()) {	Dimension viewportSize = getViewportSize();	double xScale = (double) viewportSize.width / (double) width;	double yScale = (double) viewportSize.height / (double) height;	double newScale = Math.min(xScale, yScale);	Dimension userSize = getUserSize();	this.scale = newScale;	setUserSize(userSize);	revalidate();	setViewPosition(	new Point((int) (x * getScale()), (int) (y * getScale())));	forceRedraw();	}	else {	throw new RuntimeException	("zooming only works if this view is contained in a ScrollPane");	}	}

		public void zoom(float newScale) {	if (hasZoomSupport()) {	JViewport viewport = (JViewport) getParent();	Dimension viewportSize = viewport.getSize();	Dimension userSize = getUserSize();	scale = newScale;	Point viewOrg = viewport.getViewPosition();	viewOrg.x = viewOrg.x + (viewportSize.width / 2);	viewOrg.y = viewOrg.y + (viewportSize.height / 2);	int xScreen = (int) (viewOrg.x * scale);	int yScreen = (int) (viewOrg.y * scale);	int xOrigin = xScreen - viewportSize.width / 2;	int yOrigin = yScreen - viewportSize.height / 2;	if (xOrigin < 0) xOrigin = 0;	if (yOrigin < 0) yOrigin = 0;	setUserSize(userSize);	revalidate();	viewport.setViewPosition(new Point(xOrigin, yOrigin));	forceRedraw();	}	else {	throw new RuntimeException	("zooming only works if this view is contained in a ScrollPane");	}	}

		public void zoomOut(int x, int y) {	if (hasZoomSupport()) {	Dimension viewportSize = getViewportSize();	Dimension userSize = getUserSize();	this.scale = getScale() / getZoomSpeed();	int xScreen = (int) (x * getScale());	int yScreen = (int) (y * getScale());	int xOrigin = xScreen - viewportSize.width / 2;	int yOrigin = yScreen - viewportSize.height / 2;	if (xOrigin < 0) xOrigin = 0;	if (yOrigin < 0) yOrigin = 0;	setUserSize(userSize);	revalidate();	setViewPosition(new Point(xOrigin, yOrigin));	forceRedraw();	}	else {	throw new RuntimeException	("zooming only works if this view is contained in a ScrollPane");	}	}

		public void zoomIn(int x, int y) {	if (hasZoomSupport()) {	JViewport viewport = (JViewport) getParent();	Dimension viewportSize = viewport.getSize();	Dimension userSize = getUserSize();	this.scale = getScale() * getZoomSpeed();	int xScreen = (int) (x * getScale());	int yScreen = (int) (y * getScale());	int xOrigin = xScreen - viewportSize.width / 2;	int yOrigin = yScreen - viewportSize.height / 2;	if (xOrigin < 0) xOrigin = 0;	if (yOrigin < 0) yOrigin = 0;	setUserSize(userSize);	revalidate();	viewport.setViewPosition(new Point(xOrigin, yOrigin));	forceRedraw();	}	else {	throw new RuntimeException	("zooming only works if this view is contained in a ScrollPane");	}	}

		public void deZoom(int x, int y) {	if (hasZoomSupport()) {	Dimension viewportSize = getViewportSize();	Dimension userSize = getUserSize();	int xOrigin = x - viewportSize.width / 2;	int yOrigin = y - viewportSize.height / 2;	if (xOrigin < 0) xOrigin = 0;	if (yOrigin < 0) yOrigin = 0;	this.scale = 1.0;	setUserSize(userSize);	revalidate();	setViewPosition(new Point((int) (xOrigin),	(int) (yOrigin)));	forceRedraw();	}	else {	throw new RuntimeException	("zooming only works if this view is contained in a ScrollPane");	}	}

		public void paint(Graphics g) {	super.paint(transformGraphics(g, getScale()));	}

		public Graphics getGraphics() {	return transformGraphics(super.getGraphics(), getScale());	}

		private final Graphics transformGraphics(Graphics g, double currentScale) {	if (currentScale != 1.0) {	Graphics2D g2 = (Graphics2D) g;	g2.transform(AffineTransform.getScaleInstance(currentScale, currentScale));	} return g;	}

		protected Point constrainPoint(Point p) {	Dimension size = getSize();	p.x = Geom.range(1, (int) (size.width / getScale()), p.x);	p.y = Geom.range(1, (int) (size.height / getScale()), p.y);	if (getConstrainer() != null) {	return getConstrainer().constrainPoint(p);	}	return p;	}

		public void drawBackground(Graphics g) {	g.setColor(getBackground());	g.fillRect(0, 0,	(int) (getBounds().width / getScale()),	(int) (getBounds().height / getScale()));	}

		private void centralize(Drawing d, Dimension bounds) {	Point boundsCenter = new Point(bounds.width / 2, bounds.height / 2);	Rectangle r = ((StandardDrawing) d).displayBox();	Point drawingCenter = new Point(r.x + r.width / 2, r.y + r.height / 2);	int diffX = boundsCenter.x - drawingCenter.x;	int diffY = boundsCenter.y - drawingCenter.y;	if (diffX != 0 || diffY != 0) {	for (FigureEnumeration fe = d.figures(); fe.hasNextFigure();) {	fe.nextFigure().moveBy(diffX, diffY);	}	}	}

		private void centralize(Drawing d) {	centralize(d, getUserSize());	}

		public void setDrawing(Drawing d) {	super.setDrawing(d);	Rectangle r = ((StandardDrawing) d).displayBox();	Dimension viewportSize = new Dimension(r.width, r.height);	if (getParent() != null) {	viewportSize = getViewportSize();	}	super.setPreferredSize(viewportSize);	super.setSize(viewportSize);	revalidate();	}

		public Dimension getMinimumSize() {	return super.getSize();	}

		public Dimension getPreferredSize() {	return getMinimumSize();	}

		public void repairDamage() {	Rectangle damagedArea = getDamage();	if (damagedArea != null) {	repaint((int) (damagedArea.x * getScale()),	(int) (damagedArea.y * getScale()),	(int) (damagedArea.width * getScale()),	(int) (damagedArea.height * getScale()));	setDamage(null);	}	}

		public void drawingInvalidated(DrawingChangeEvent e) {	Rectangle r = e.getInvalidatedRectangle();	if (getDamage() == null) {	setDamage(r);	}	else {	Rectangle damagedArea = getDamage();	damagedArea.add(r);	setDamage(damagedArea);	}	}

		private MouseEvent createScaledEvent(MouseEvent e) {	return new MouseEvent(e.getComponent(),	e.getID(),	e.getWhen(),	e.getModifiers(),	(int) (e.getX() / getScale()),	(int) (e.getY() / getScale()),	e.getClickCount(),	e.isPopupTrigger());	}

		protected MouseListener createMouseListener() {	return new StandardDrawingView.DrawingViewMouseListener() {	public void mousePressed(MouseEvent e) {	super.mousePressed(createScaledEvent(e));	}	public void mouseReleased(MouseEvent e) {	super.mouseReleased(createScaledEvent(e));	}	};	}

		protected MouseMotionListener createMouseMotionListener() {	return new StandardDrawingView.DrawingViewMouseMotionListener() {	public void mouseDragged(MouseEvent e) {	super.mouseDragged(createScaledEvent(e));	}	public void mouseMoved(MouseEvent e) {	super.mouseMoved(createScaledEvent(e));	}	};	}

		protected KeyListener createKeyListener() {	return new StandardDrawingView.DrawingViewKeyListener() {	public void keyPressed(KeyEvent e) {	super.keyPressed(e);	if (e.getKeyChar() == ' ') {	forceRedraw();	}	else if (e.getKeyChar() == 'o') {	setScale(getScale() / getZoomSpeed());	}	else if (e.getKeyChar() == 'i') {	setScale(getScale() * getZoomSpeed());	}	else if (e.getKeyChar() == 'c') {	centralize(drawing());	}	else {	super.keyPressed(e);	}	}	};	}

		public double getZoomSpeed()	{	return zoomSpeed;	}

		public void setZoomSpeed(double newZoomSpeed)	{	zoomSpeed = Math.max(1.1, newZoomSpeed);	}


}
