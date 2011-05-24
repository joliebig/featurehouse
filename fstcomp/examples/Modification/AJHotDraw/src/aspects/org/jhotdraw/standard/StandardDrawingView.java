
package org.jhotdraw.standard; 
import java.awt.*; 
import java.awt.dnd.DragGestureListener; 
import java.awt.dnd.DragSourceListener; 
import java.awt.event.*; 
import java.io.IOException; 
import java.io.ObjectInputStream; 
import java.util.Collection; 
import java.util.Iterator; 
import java.util.List; 
import javax.swing.JOptionPane; 
import javax.swing.JPanel; 
import org.jhotdraw.contrib.AutoscrollHelper; 
import org.jhotdraw.contrib.dnd.DNDHelper; 
import org.jhotdraw.contrib.dnd.DNDInterface; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.framework.Cursor; 
import org.jhotdraw.util.CollectionsFactory; 
import org.jhotdraw.util.Command; 
import org.jhotdraw.util.Geom; 
public  class  StandardDrawingView 	extends JPanel 	implements DrawingView, DNDInterface, java.awt.dnd.Autoscroll {
		transient private DrawingEditor fEditor;

		private Drawing fDrawing;

		private transient Rectangle fDamage;

		transient private List fSelection;

		transient private List fSelectionHandles;

		private Point fLastClick;

		private List fBackgrounds;

		private List fForegrounds;

		private Painter fUpdateStrategy;

		private PointConstrainer fConstrainer;

		public static final int MINIMUM_WIDTH = 400;

		public static final int MINIMUM_HEIGHT = 300;

		public static final int SCROLL_INCR = 100;

		public static final int SCROLL_OFFSET = 10;

		private static int counter;

		private int myCounter = counter;

		private DNDHelper dndh;

	 private MouseListener mouseListener;

	 private MouseMotionListener motionListener;

	 private KeyListener keyListener;

	 private boolean myIsReadOnly;

		private static final long serialVersionUID = -3878153366174603336L;

		private int drawingViewSerializedDataVersion = 1;

		public StandardDrawingView(DrawingEditor editor) {	this(editor, MINIMUM_WIDTH, MINIMUM_HEIGHT);	}

		public StandardDrawingView(DrawingEditor editor, int width, int height) {	setAutoscrolls(true);	counter++;	fEditor = editor;	setPreferredSize(new Dimension(width, height));	setLastClick(new Point(0, 0));	fConstrainer = null;	fSelection = CollectionsFactory.current().createList();	setDisplayUpdate(createDisplayUpdate());	setBackground(Color.lightGray);	addMouseListener(createMouseListener());	addMouseMotionListener(createMouseMotionListener());	addKeyListener(createKeyListener()); }

		protected MouseListener createMouseListener() { mouseListener = new DrawingViewMouseListener();	return mouseListener;	}

		protected MouseMotionListener createMouseMotionListener() { motionListener = new DrawingViewMouseMotionListener();	return motionListener;	}

		protected KeyListener createKeyListener() { keyListener = new DrawingViewKeyListener();	return keyListener;	}

		protected Painter createDisplayUpdate() {	return new SimpleUpdateStrategy();	}

		public void setEditor(DrawingEditor editor) {	fEditor = editor;	}

		public Tool tool() {	return editor().tool();	}

		public Drawing drawing() {	return fDrawing;	}

		public void setDrawing(Drawing d) {	if (drawing() != null) {	clearSelection();	drawing().removeDrawingChangeListener(this);	}	fDrawing = d;	if (drawing() != null) {	drawing().addDrawingChangeListener(this);	}	checkMinimumSize();	repaint();	}

		public DrawingEditor editor() {	return fEditor;	}

		public Figure add(Figure figure) {	return drawing().add(figure);	}

		public Figure remove(Figure figure) {	return drawing().remove(figure);	}

		public void addAll(Collection figures) {	FigureEnumeration fe = new FigureEnumerator(figures);	while (fe.hasNextFigure()) {	add(fe.nextFigure());	}	}

		public boolean figureExists(Figure inf, FigureEnumeration fe) {	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	if (figure.includes(inf)) {	return true;	}	} return false;	}

		public FigureEnumeration insertFigures(FigureEnumeration fe, int dx, int dy, boolean bCheck) {	if (fe == null) {	return FigureEnumerator.getEmptyEnumeration();	}	List vCF = CollectionsFactory.current().createList(10);	InsertIntoDrawingVisitor visitor = new InsertIntoDrawingVisitor(drawing());	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	if (figure instanceof ConnectionFigure) {	vCF.add(figure);	}	else if (figure != null) {	figure.moveBy(dx, dy);	figure.visit(visitor);	}	}	FigureEnumeration ecf = new FigureEnumerator(vCF);	while (ecf.hasNextFigure()) {	ConnectionFigure cf = (ConnectionFigure) ecf.nextFigure();	Figure sf = cf.startFigure();	Figure ef = cf.endFigure();	if (figureExists(sf, drawing().figures())	&& figureExists(ef, drawing().figures())	&& (!bCheck || cf.canConnect(sf, ef))) {	if (bCheck) {	Point sp = sf.center();	Point ep = ef.center();	Connector fStartConnector = cf.startFigure().connectorAt(ep.x, ep.y);	Connector fEndConnector = cf.endFigure().connectorAt(sp.x, sp.y);	if (fEndConnector != null && fStartConnector != null) {	cf.connectStart(fStartConnector);	cf.connectEnd(fEndConnector);	cf.updateConnection();	}	}	cf.visit(visitor);	}	}	addToSelectionAll(visitor.getInsertedFigures());	return visitor.getInsertedFigures();	}

		public FigureEnumeration getConnectionFigures(Figure inFigure) {	if (inFigure == null || !inFigure.canConnect()) {	return null;	}	List result = CollectionsFactory.current().createList(5);	FigureEnumeration figures = drawing().figures();	while (figures.hasNextFigure()) {	Figure f= figures.nextFigure();	if ((f instanceof ConnectionFigure) && !(isFigureSelected(f))) {	ConnectionFigure cf = (ConnectionFigure) f;	if (cf.startFigure().includes(inFigure) || cf.endFigure().includes(inFigure)) {	result.add(f);	}	}	}	return new FigureEnumerator(result); }

		public void setDisplayUpdate(Painter updateStrategy) {	fUpdateStrategy = updateStrategy;	}

		public Painter getDisplayUpdate() {	return fUpdateStrategy;	}

		public FigureEnumeration selection() {	return selectionZOrdered();	}

		public FigureEnumeration selectionZOrdered() {	List result = CollectionsFactory.current().createList(selectionCount());	result.addAll(fSelection);	return new ReverseFigureEnumerator(result);	}

		public int selectionCount() {	return fSelection.size();	}

		public boolean isFigureSelected(Figure checkFigure) {	return fSelection.contains(checkFigure);	}

		public void addToSelection(Figure figure) { addToSelectionImpl(figure);	}

		protected boolean addToSelectionImpl(Figure figure){	boolean changed = false;	if (!isFigureSelected(figure) && drawing().includes(figure)) {	fSelection.add(figure);	fSelectionHandles = null;	figure.invalidate();	changed = true;	}	return changed;	}

		public void addToSelectionAll(Collection figures) {	addToSelectionAll(new FigureEnumerator(figures));	}

		public void addToSelectionAll(FigureEnumeration fe) {	boolean changed = false;	while (fe.hasNextFigure()) {	changed |= addToSelectionImpl(fe.nextFigure());	}	}

		public void removeFromSelection(Figure figure) {	if (isFigureSelected(figure)) {	fSelection.remove(figure);	fSelectionHandles = null;	figure.invalidate();	}	}

		public void toggleSelection(Figure figure) {	if (isFigureSelected(figure)) {	removeFromSelection(figure);	}	else {	addToSelection(figure);	}	}

		public void clearSelection() {	if (selectionCount() == 0) {	return;	}	FigureEnumeration fe = selection();	while (fe.hasNextFigure()) {	fe.nextFigure().invalidate();	}	fSelection = CollectionsFactory.current().createList();	fSelectionHandles = null;	}

		protected HandleEnumeration selectionHandles() {	if (fSelectionHandles == null) {	fSelectionHandles = CollectionsFactory.current().createList();	FigureEnumeration fe = selection();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	HandleEnumeration kk = figure.handles();	while (kk.hasNextHandle()) {	fSelectionHandles.add(kk.nextHandle());	}	}	}	return new HandleEnumerator(fSelectionHandles);	}

		public FigureSelection getFigureSelection() {	return new StandardFigureSelection(selectionZOrdered(), selectionCount());	}

		public Handle findHandle(int x, int y) {	Handle handle;	HandleEnumeration he = selectionHandles();	while (he.hasNextHandle()) {	handle = he.nextHandle();	if (handle.containsPoint(x, y)) {	return handle;	}	}	return null;	}

	 protected Rectangle getDamage() { return fDamage; }

	 protected void setDamage(Rectangle r) { fDamage = r; }

		public Point lastClick() {	return fLastClick;	}

		protected void setLastClick(Point newLastClick) {	fLastClick = newLastClick;	}

		public void setConstrainer(PointConstrainer c) {	fConstrainer = c;	}

		public PointConstrainer getConstrainer() {	return fConstrainer;	}

		protected Point constrainPoint(Point p) {	Dimension size = getSize();	p.x = Geom.range(1, size.width, p.x);	p.y = Geom.range(1, size.height, p.y);	if (fConstrainer != null ) {	return fConstrainer.constrainPoint(p);	}	return p;	}

		private void moveSelection(int dx, int dy) {	FigureEnumeration figures = selection();	while (figures.hasNextFigure()) {	figures.nextFigure().moveBy(dx, dy);	}	checkDamage();	}

		public synchronized void checkDamage() {	Iterator each = drawing().drawingChangeListeners();	while (each.hasNext()) {	Object l = each.next();	if (l instanceof DrawingView) {	((DrawingView)l).repairDamage();	}	}	}

		public void repairDamage() {	if (getDamage() != null) {	repaint(getDamage().x, getDamage().y, getDamage().width, getDamage().height);	setDamage(null);	}	}

		public void drawingInvalidated(DrawingChangeEvent e) {	Rectangle r = e.getInvalidatedRectangle();	if (getDamage() == null) {	setDamage(r);	}	else {	Rectangle damagedR = getDamage();	damagedR.add(r);	setDamage(damagedR);	}	}

		public void drawingRequestUpdate(DrawingChangeEvent e) {	repairDamage();	}

		public void drawingTitleChanged(DrawingChangeEvent e){	}

		protected void paintComponent(Graphics g) {	if(getDisplayUpdate() != null) {	getDisplayUpdate().draw(g, this);	}	}

		public void drawAll(Graphics g) {	boolean isPrinting = g instanceof PrintGraphics;	drawBackground(g);	if ((fBackgrounds != null) && !isPrinting) {	drawPainters(g, fBackgrounds);	}	drawDrawing(g);	if ((fForegrounds != null) && !isPrinting) {	drawPainters(g, fForegrounds);	}	if (!isPrinting) {	drawHandles(g);	}	}

	 public void draw(Graphics g, FigureEnumeration fe) {	boolean isPrinting = g instanceof PrintGraphics;	if ((fBackgrounds != null) && !isPrinting) {	drawPainters(g, fBackgrounds);	}	drawing().draw(g, fe);	if ((fForegrounds != null) && !isPrinting) {	drawPainters(g, fForegrounds);	}	if (!isPrinting) {	drawHandles(g);	}	}

		public void drawHandles(Graphics g) {	HandleEnumeration he = selectionHandles();	while (he.hasNextHandle()) {	(he.nextHandle()).draw(g);	}	}

		public void drawDrawing(Graphics g) {	drawing().draw(g);	}

		public void drawBackground(Graphics g) {	g.setColor(getBackground());	g.fillRect(0, 0, getBounds().width, getBounds().height);	}

		protected void drawPainters(Graphics g, List v) {	for (int i = 0; i < v.size(); i++) {	((Painter)v.get(i)).draw(g, this);	}	}

		public void addBackground(Painter painter) {	if (fBackgrounds == null) {	fBackgrounds = CollectionsFactory.current().createList(3);	}	fBackgrounds.add(painter);	repaint();	}

		public void removeBackground(Painter painter) {	if (fBackgrounds != null) {	fBackgrounds.remove(painter);	}	repaint();	}

	 protected List getBackgrounds() { return fBackgrounds; }

		public void removeForeground(Painter painter) {	if (fForegrounds != null) {	fForegrounds.remove(painter);	}	repaint();	}

		public void addForeground(Painter painter) {	if (fForegrounds == null) {	fForegrounds = CollectionsFactory.current().createList(3);	}	fForegrounds.add(painter);	repaint();	}

	 protected List getForegrounds() { return fForegrounds; }

		public void freezeView() {	drawing().lock();	}

		public void unfreezeView() {	drawing().unlock();	}

		private void readObject(ObjectInputStream s)	throws ClassNotFoundException, IOException {	s.defaultReadObject();	fSelection = CollectionsFactory.current().createList();	if (drawing() != null) {	drawing().addDrawingChangeListener(this);	}	}

	 protected void checkMinimumSize() { Dimension d = getDrawingSize();	Dimension v = getPreferredSize();	if (v.height < d.height || v.width < d.width) {	v.height = d.height + SCROLL_OFFSET;	v.width = d.width + SCROLL_OFFSET;	setPreferredSize(v); } }

	 protected Dimension getDrawingSize() {	Dimension d = new Dimension(0, 0);	if (drawing() != null) {	FigureEnumeration fe = drawing().figures();	while (fe.hasNextFigure()) {	Rectangle r = fe.nextFigure().displayBox();	d.width = Math.max(d.width, r.x+r.width);	d.height = Math.max(d.height, r.y+r.height);	}	} return d;	}

		public boolean isFocusTraversable() {	return true;	}

		public boolean isInteractive() {	return true;	}

		public void keyTyped(KeyEvent e) {}

		public void keyReleased(KeyEvent e) {}

		public int getDefaultDNDActions() {	return java.awt.dnd.DnDConstants.ACTION_COPY_OR_MOVE;	}

		private ASH ash = new ASH(10);

		public void autoscroll(java.awt.Point p) {	ash.autoscroll(p);	}

		public Insets getAutoscrollInsets() {	return ash.getAutoscrollInsets();	}

		 	class  ASH  extends AutoscrollHelper {
			public ASH(int margin) {	super(margin);	}

			public Dimension getSize() {	return StandardDrawingView.this.getSize();	}

			public Rectangle getVisibleRect() {	return StandardDrawingView.this.getVisibleRect();	}

			public void scrollRectToVisible(Rectangle aRect) {	StandardDrawingView.this.scrollRectToVisible(aRect);	}


	}

		public String toString() {	return "DrawingView Nr: " + myCounter;	}

	 protected void handleMouseEventException(Throwable t) {	JOptionPane.showMessageDialog(	this, t.getClass().getName() + " - " + t.getMessage(),	"Error",	JOptionPane.ERROR_MESSAGE);	t.printStackTrace(); }

		public  class  DrawingViewMouseListener  extends MouseAdapter {
			public void mousePressed(MouseEvent e) {	try {	requestFocus();	Point p = constrainPoint(new Point(e.getX(), e.getY()));	setLastClick(new Point(e.getX(), e.getY()));	tool().mouseDown(e, p.x, p.y);	checkDamage();	}	catch (Throwable t) {	handleMouseEventException(t);	}	}

			public void mouseReleased(MouseEvent e) {	try {	Point p = constrainPoint(new Point(e.getX(), e.getY()));	tool().mouseUp(e, p.x, p.y);	checkDamage();	}	catch (Throwable t) {	handleMouseEventException(t);	}	}


	}

		public  class  DrawingViewMouseMotionListener  implements MouseMotionListener {
			public void mouseDragged(MouseEvent e) {	try {	Point p = constrainPoint(new Point(e.getX(), e.getY()));	tool().mouseDrag(e, p.x, p.y);	checkDamage();	}	catch (Throwable t) {	handleMouseEventException(t);	}	}

			public void mouseMoved(MouseEvent e) {	try {	tool().mouseMove(e, e.getX(), e.getY());	}	catch (Throwable t) {	handleMouseEventException(t);	}	}


	}

		public  class  DrawingViewKeyListener  implements KeyListener {
			private Command deleteCmd;

			public DrawingViewKeyListener() {	deleteCmd = createDeleteCommand();	}

			public void keyPressed(KeyEvent e) {	int code = e.getKeyCode();	int modifiers = e.getModifiers();	if (modifiers == 0 && ((code == KeyEvent.VK_BACK_SPACE) || (code == KeyEvent.VK_DELETE))) {	if (deleteCmd.isExecutable()) {	deleteCmd.execute();	}	}	else if (modifiers == 0 && ((code == KeyEvent.VK_DOWN) || (code == KeyEvent.VK_UP) || (code == KeyEvent.VK_RIGHT) || (code == KeyEvent.VK_LEFT))) {	handleCursorKey(code);	}	else {	tool().keyDown(e, code);	}	checkDamage();	}

			protected void handleCursorKey(int key) {	int dx = 0, dy = 0;	int stepX = 1, stepY = 1;	if (fConstrainer != null) {	stepX = fConstrainer.getStepX();	stepY = fConstrainer.getStepY();	}	switch (key) {	case KeyEvent.VK_DOWN:	dy = stepY;	break;	case KeyEvent.VK_UP:	dy = -stepY;	break;	case KeyEvent.VK_RIGHT:	dx = stepX;	break;	case KeyEvent.VK_LEFT:	dx = -stepX;	break;	}	moveSelection(dx, dy);	}

		 public void keyTyped(KeyEvent event) { }

		 public void keyReleased(KeyEvent event) { }

			protected Command createDeleteCommand() {	return new DeleteCommand("Delete", editor())/*)*/;	}


	}

		protected DNDHelper createDNDHelper() {	return new DNDHelper(true, true) {	protected DrawingView view() {	return StandardDrawingView.this;	}	protected DrawingEditor editor() {	return StandardDrawingView.this.editor();	}	};	}

		protected DNDHelper getDNDHelper() {	if (dndh == null) {	dndh = createDNDHelper();	}	return dndh;	}

		public DragSourceListener getDragSourceListener(){	return getDNDHelper().getDragSourceListener();	}

		public void DNDInitialize(DragGestureListener dgl){	getDNDHelper().initialize(dgl);	}

		public void DNDDeinitialize() {	getDNDHelper().deinitialize();	}

	 public boolean isReadOnly() { return myIsReadOnly; }

	 public void setReadOnly(boolean newIsReadOnly) { if (newIsReadOnly != isReadOnly()) { if (newIsReadOnly) { removeMouseListener(mouseListener); removeMouseMotionListener(motionListener); removeKeyListener(keyListener); } else { addMouseListener(mouseListener); addMouseMotionListener(motionListener); addKeyListener(keyListener); } myIsReadOnly = newIsReadOnly; } }

		public void setCursor(Cursor cursor) {	if (cursor instanceof java.awt.Cursor) {	super.setCursor((java.awt.Cursor) cursor);	}	}

		public Dimension getMinimumSize() {	Rectangle r = new Rectangle();	FigureEnumeration k = drawing().figures();	while (k.hasNextFigure()) {	r.add(k.nextFigure().displayBox());	}	return new Dimension(r.width, r.height);	}


}
