
package org.jhotdraw.figures; 
import java.awt.Color; 
import java.awt.Graphics; 
import java.awt.Point; 
import java.awt.Rectangle; 
import java.util.Iterator; 
import java.util.List; 
import org.jhotdraw.framework.Connector; 
import org.jhotdraw.framework.FigureAttributeConstant; 
import org.jhotdraw.framework.HandleEnumeration; 
import org.jhotdraw.framework.Locator; 
import org.jhotdraw.standard.AbstractFigure; 
import org.jhotdraw.standard.HandleEnumerator; 
import org.jhotdraw.util.CollectionsFactory; 
import org.jhotdraw.util.Geom; 
public  class  PolyLineFigure  extends AbstractFigure {
		public final static int ARROW_TIP_NONE = 0;

		public final static int ARROW_TIP_START = 1;

		public final static int ARROW_TIP_END = 2;

		public final static int ARROW_TIP_BOTH = 3;

		protected List fPoints;

		protected LineDecoration fStartDecoration = null;

		protected LineDecoration fEndDecoration = null;

		protected Color fFrameColor = Color.black;

		private static final long serialVersionUID = -7951352179906577773L;

		private int polyLineFigureSerializedDataVersion = 1;

		public PolyLineFigure() {	this(4);	}

		public PolyLineFigure(int size) {	fPoints = CollectionsFactory.current().createList(size);	}

		public PolyLineFigure(int x, int y) {	fPoints = CollectionsFactory.current().createList();	fPoints.add(new Point(x, y));	}

		public Rectangle displayBox() {	Iterator iter = points();	if (iter.hasNext()) {	Rectangle r = new Rectangle((Point)iter.next());	while (iter.hasNext()) {	r.add((Point)iter.next());	}	return r;	}	else {	return new Rectangle();	}	}

		public boolean isEmpty() {	return (size().width < 3) && (size().height < 3);	}

		public HandleEnumeration handles() {	List handles = CollectionsFactory.current().createList(fPoints.size());	for (int i = 0; i < fPoints.size(); i++) {	handles.add(new PolyLineHandle(this, locator(i), i));	}	return new HandleEnumerator(handles);	}

		public void basicDisplayBox(Point origin, Point corner) {	}

		public void addPoint(int x, int y) {	fPoints.add(new Point(x, y));	changed();	}

		public Iterator points() {	return fPoints.iterator();	}

		public int pointCount() {	return fPoints.size();	}

		protected void basicMoveBy(int dx, int dy) {	Iterator iter = points();	while (iter.hasNext()) {	((Point)iter.next()).translate(dx, dy);	}	}

		public void setPointAt(Point p, int i) {	willChange();	fPoints.set(i, p);	changed();	}

		public void insertPointAt(Point p, int i) {	fPoints.add(i, p);	changed();	}

		public void removePointAt(int i) {	willChange();	fPoints.remove(i);	changed();	}

		public int splitSegment(int x, int y) {	int i = findSegment(x, y);	if (i != -1) {	insertPointAt(new Point(x, y), i+1);	}	return i+1;	}

		public Point pointAt(int i) {	return (Point)fPoints.get(i);	}

		public boolean joinSegments(int x, int y) {	for (int i= 1; i < fPoints.size()-1; i++) {	Point p = pointAt(i);	if (Geom.length(x, y, p.x, p.y) < 3) {	removePointAt(i);	return true;	}	}	return false;	}

		public Connector connectorAt(int x, int y) {	return new PolyLineConnector(this);	}

		public void setStartDecoration(LineDecoration l) {	fStartDecoration = l;	}

		public LineDecoration getStartDecoration() {	return fStartDecoration;	}

		public void setEndDecoration(LineDecoration l) {	fEndDecoration = l;	}

		public LineDecoration getEndDecoration() {	return fEndDecoration;	}

		public void draw(Graphics g) {	g.setColor(getFrameColor());	Point p1, p2;	for (int i = 0; i < fPoints.size()-1; i++) {	p1 = pointAt(i);	p2 = pointAt(i+1);	drawLine(g, p1.x, p1.y, p2.x, p2.y);	}	decorate(g);	}

		protected void drawLine(Graphics g, int x1, int y1, int x2, int y2) {	g.drawLine(x1, y1, x2, y2);	}

		public boolean containsPoint(int x, int y) {	Rectangle bounds = displayBox();	bounds.grow(4,4);	if (!bounds.contains(x, y)) {	return false;	}	for (int i = 0; i < fPoints.size()-1; i++) {	Point p1 = pointAt(i);	Point p2 = pointAt(i+1);	if (Geom.lineContainsPoint(p1.x, p1.y, p2.x, p2.y, x, y)) {	return true;	}	}	return false;	}

		public int findSegment(int x, int y) {	for (int i = 0; i < fPoints.size()-1; i++) {	Point p1 = pointAt(i);	Point p2 = pointAt(i+1);	if (Geom.lineContainsPoint(p1.x, p1.y, p2.x, p2.y, x, y)) {	return i;	}	}	return -1;	}

		private void decorate(Graphics g) {	if (getStartDecoration() != null) {	Point p1 = pointAt(0);	Point p2 = pointAt(1);	getStartDecoration().draw(g, p1.x, p1.y, p2.x, p2.y);	}	if (getEndDecoration() != null) {	Point p3 = pointAt(fPoints.size()-2);	Point p4 = pointAt(fPoints.size()-1);	getEndDecoration().draw(g, p4.x, p4.y, p3.x, p3.y);	}	}

		public Object getAttribute(String name) {	return getAttribute(FigureAttributeConstant.getConstant(name));	}

		public Object getAttribute(FigureAttributeConstant attributeConstant) {	if (attributeConstant.equals(FigureAttributeConstant.FRAME_COLOR)) {	return getFrameColor();	}	else if (attributeConstant.equals(FigureAttributeConstant.ARROW_MODE)) {	int value = 0;	if (getStartDecoration() != null) {	value |= ARROW_TIP_START;	}	if (getEndDecoration() != null) {	value |= ARROW_TIP_END;	}	return new Integer(value);	}	return super.getAttribute(attributeConstant);	}

		public void setAttribute(String name, Object value) {	setAttribute(FigureAttributeConstant.getConstant(name), value);	}

		public void setAttribute(FigureAttributeConstant attributeConstant, Object value) {	if (attributeConstant.equals(FigureAttributeConstant.FRAME_COLOR)) {	setFrameColor((Color)value);	changed();	}	else if (attributeConstant.equals(FigureAttributeConstant.ARROW_MODE)) {	Integer intObj = (Integer)value;	if (intObj != null) {	int decoration = intObj.intValue();	if ((decoration & ARROW_TIP_START) != 0) {	setStartDecoration(new ArrowTip());	}	else {	setStartDecoration(null);	}	if ((decoration & ARROW_TIP_END) != 0) {	setEndDecoration(new ArrowTip());	}	else {	setEndDecoration(null);	}	}	changed();	}	else {	super.setAttribute(attributeConstant, value);	}	}

		public static Locator locator(int pointIndex) {	return new PolyLineLocator(pointIndex);	}

		protected Color getFrameColor() {	return fFrameColor;	}

		protected void setFrameColor(Color c) {	fFrameColor = c;	}

		protected Rectangle invalidateRectangle(Rectangle r) {	Rectangle parentR = super.invalidateRectangle(r);	if (getStartDecoration() != null) {	parentR.add(getStartDecoration().displayBox());	}	if (getEndDecoration() != null) {	parentR.add(getEndDecoration().displayBox());	}	return parentR;	}


}
