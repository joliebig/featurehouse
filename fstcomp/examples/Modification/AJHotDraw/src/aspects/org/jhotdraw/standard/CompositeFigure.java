
package org.jhotdraw.standard; 
import org.jhotdraw.util.*; 
import org.jhotdraw.framework.*; 
import java.awt.*; 
import java.util.*; 
import java.util.List; 
import java.io.*; 
public abstract  class  CompositeFigure 	extends AbstractFigure 	implements FigureChangeListener {
		protected List fFigures;

		private static final long serialVersionUID = 7408153435700021866L;

		private int compositeFigureSerializedDataVersion = 1;

		private transient QuadTree _theQuadTree;

		protected int _nLowestZ;

		protected int _nHighestZ;

		protected CompositeFigure() {	fFigures = CollectionsFactory.current().createList();	_nLowestZ = 0;	_nHighestZ = 0;	}

		public Figure add(Figure figure) {	if (!containsFigure(figure)) {	figure.setZValue(++_nHighestZ);	fFigures.add(figure);	figure.addToContainer(this);	_addToQuadTree(figure);	}	return figure;	}

		public void addAll(List newFigures) {	addAll(new FigureEnumerator(newFigures));	}

		public void addAll(FigureEnumeration fe) {	while (fe.hasNextFigure()) {	add(fe.nextFigure());	}	}

		public Figure remove(Figure figure) {	Figure orphanedFigure = orphan(figure);	if (orphanedFigure != null) {	orphanedFigure.release();	}	return orphanedFigure;	}

		public void removeAll(List figures) {	removeAll(new FigureEnumerator(figures));	}

		public void removeAll(FigureEnumeration fe) {	while (fe.hasNextFigure()) {	remove(fe.nextFigure());	}	}

		public void removeAll() {	FigureEnumeration fe = figures();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	figure.removeFromContainer(this);	}	fFigures.clear();	_clearQuadTree();	_nLowestZ = 0;	_nHighestZ = 0;	}

		public synchronized Figure orphan(Figure figure) {	figure.removeFromContainer(this);	fFigures.remove(figure);	_removeFromQuadTree(figure);	return figure;	}

		public void orphanAll(List newFigures) {	orphanAll(new FigureEnumerator(newFigures));	}

		public void orphanAll(FigureEnumeration fe) {	while (fe.hasNextFigure()) {	orphan(fe.nextFigure());	}	}

		public synchronized Figure replace(Figure figure, Figure replacement) {	int index = fFigures.indexOf(figure);	if (index != -1) {	replacement.setZValue(figure.getZValue());	replacement.addToContainer(this);	figure.removeFromContainer(this);	fFigures.set(index, replacement);	figure.changed();	replacement.changed();	}	return replacement;	}

		public synchronized void sendToBack(Figure figure) {	if (containsFigure(figure)) {	fFigures.remove(figure);	fFigures.add(0, figure);	_nLowestZ--;	figure.setZValue(_nLowestZ);	figure.changed();	}	}

		public synchronized void bringToFront(Figure figure) {	if (containsFigure(figure)) {	fFigures.remove(figure);	fFigures.add(figure);	_nHighestZ++;	figure.setZValue(_nHighestZ);	figure.changed();	}	}

		public void sendToLayer(Figure figure, int layerNr) {	if (containsFigure(figure)) {	if (layerNr >= fFigures.size()) {	layerNr = fFigures.size() - 1;	}	Figure layerFigure = getFigureFromLayer(layerNr);	int layerFigureZValue = layerFigure.getZValue();	int figureLayer = getLayer(figure);	if (figureLayer < layerNr) {	assignFiguresToPredecessorZValue(figureLayer + 1, layerNr);	}	else if (figureLayer > layerNr) {	assignFiguresToSuccessorZValue(layerNr, figureLayer - 1);	}	fFigures.remove(figure);	fFigures.add(layerNr, figure);	figure.setZValue(layerFigureZValue);	figure.changed();	}	}

		private void assignFiguresToPredecessorZValue(int lowerBound, int upperBound) {	if (upperBound >= fFigures.size()) {	upperBound = fFigures.size() - 1;	}	for (int i = upperBound; i >= lowerBound; i--) {	Figure currentFigure = (Figure)fFigures.get(i);	Figure predecessorFigure = (Figure)fFigures.get(i - 1);	currentFigure.setZValue(predecessorFigure.getZValue());	}	}

		private void assignFiguresToSuccessorZValue(int lowerBound, int upperBound) {	if (upperBound >= fFigures.size()) {	upperBound = fFigures.size() - 1;	}	for (int i = upperBound; i >= lowerBound; i--) {	Figure currentFigure = (Figure)fFigures.get(i);	Figure successorFigure = (Figure)fFigures.get(i + 1);	currentFigure.setZValue(successorFigure.getZValue());	}	}

		public int getLayer(Figure figure) {	if (!containsFigure(figure)) {	return -1;	}	else {	return fFigures.indexOf(figure);	}	}

		public Figure getFigureFromLayer(int layerNr) {	if ((layerNr >= 0) && (layerNr < fFigures.size())) {	return (Figure)fFigures.get(layerNr);	}	else {	return null;	}	}

		public void draw(Graphics g) {	draw(g, figures());	}

		public void draw(Graphics g, FigureEnumeration fe) {	while (fe.hasNextFigure()) {	fe.nextFigure().draw(g);	}	}

		public Figure figureAt(int i) {	return (Figure)fFigures.get(i);	}

		public FigureEnumeration figures() {	return new FigureEnumerator(CollectionsFactory.current().createList(fFigures));	}

		public FigureEnumeration figures(Rectangle viewRectangle) {	if (_theQuadTree != null) {	FigureEnumeration fe =	_theQuadTree.getAllWithin(new Bounds(viewRectangle).asRectangle2D());	List l2 = CollectionsFactory.current().createList();	while (fe.hasNextFigure()) {	Figure f = fe.nextFigure();	l2.add(new OrderedFigureElement(f, f.getZValue()));	}	Collections.sort(l2);	List l3 = CollectionsFactory.current().createList();	for (Iterator iter = l2.iterator(); iter.hasNext(); ) {	OrderedFigureElement ofe = (OrderedFigureElement)iter.next();	l3.add(ofe.getFigure());	}	return new FigureEnumerator(l3);	}	return figures();	}

		public int figureCount() {	return fFigures.size();	}

		public boolean containsFigure(Figure checkFigure) {	return fFigures.contains(checkFigure);	}

		public final FigureEnumeration figuresReverse() {	return new ReverseFigureEnumerator(CollectionsFactory.current().createList(fFigures));	}

		public Figure findFigure(int x, int y) {	FigureEnumeration fe = figuresReverse();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	if (figure.containsPoint(x, y)) {	return figure;	}	}	return null;	}

		public Figure findFigure(Rectangle r) {	FigureEnumeration fe = figuresReverse();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	Rectangle fr = figure.displayBox();	if (r.intersects(fr)) {	return figure;	}	}	return null;	}

		public Figure findFigureWithout(int x, int y, Figure without) {	if (without == null) {	return findFigure(x, y);	}	FigureEnumeration fe = figuresReverse();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	if (figure.containsPoint(x, y) && !figure.includes(without)) {	return figure;	}	}	return null;	}

		public Figure findFigure(Rectangle r, Figure without) {	if (without == null) {	return findFigure(r);	}	FigureEnumeration fe = figuresReverse();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	Rectangle fr = figure.displayBox();	if (r.intersects(fr) && !figure.includes(without)) {	return figure;	}	}	return null;	}

		public Figure findFigureInside(int x, int y) {	FigureEnumeration fe = figuresReverse();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure().findFigureInside(x, y);	if (figure != null) {	return figure;	}	}	if (containsPoint(x, y)) {	return this;	}	else {	return null;	}	}

		public Figure findFigureInsideWithout(int x, int y, Figure without) {	if (without == null) {	return findFigureInside(x, y);	}	FigureEnumeration fe = figuresReverse();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	if (figure != without) {	Figure found = figure.findFigureInside(x, y);	if ((found != null) && !figure.includes(without)) {	return found;	}	}	}	if (containsPoint(x, y)) {	return this;	}	else {	return null;	}	}

		public boolean includes(Figure figure) {	if (super.includes(figure)) {	return true;	}	FigureEnumeration fe = figures();	while (fe.hasNextFigure()) {	Figure f = fe.nextFigure();	if (f.includes(figure)) {	return true;	}	}	return false;	}

		protected void basicMoveBy(int x, int y) {	FigureEnumeration fe = figures();	while (fe.hasNextFigure()) {	fe.nextFigure().moveBy(x,y);	}	}

		public void release() {	FigureEnumeration fe = figures();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	figure.release();	}	super.release();	}

		public void figureInvalidated(FigureChangeEvent e) {	if (listener() != null) {	listener().figureInvalidated(e);	}	}

		public void figureRequestRemove(FigureChangeEvent e) {	if (listener() != null) {	listener().figureRequestRemove(new FigureChangeEvent(this));	}	}

		public void figureRequestUpdate(FigureChangeEvent e) {	if (listener() != null) {	listener().figureRequestUpdate(e);	}	}

		public void figureChanged(FigureChangeEvent e) {	_removeFromQuadTree(e.getFigure());	_addToQuadTree(e.getFigure());	}

		public void figureRemoved(FigureChangeEvent e) {	if (listener() != null) {	listener().figureRemoved(e);	}	}

		private void readObject(ObjectInputStream s)	throws ClassNotFoundException, IOException {	s.defaultReadObject();	FigureEnumeration fe = figures();	while (fe.hasNextFigure()) {	Figure figure = fe.nextFigure();	figure.addToContainer(this);	}	init(new Rectangle(0, 0));	}

		public void init(Rectangle viewRectangle) {	_theQuadTree = new QuadTree(new Bounds(viewRectangle).asRectangle2D());	FigureEnumeration fe = figures();	while (fe.hasNextFigure()) {	_addToQuadTree(fe.nextFigure());	}	}

		private void _addToQuadTree(Figure f) {	if (_theQuadTree != null) {	Rectangle r = f.displayBox();	if (r.height == 0) {	r.grow(0, 1);	}	if (r.width == 0) {	r.grow(1, 0);	}	_theQuadTree.add(f, new Bounds(r).asRectangle2D());	}	}

		private void _removeFromQuadTree(Figure f) {	if (_theQuadTree != null) {	_theQuadTree.remove(f);	}	}

		private void _clearQuadTree() {	if (_theQuadTree != null) {	_theQuadTree.clear();	}	}


}
