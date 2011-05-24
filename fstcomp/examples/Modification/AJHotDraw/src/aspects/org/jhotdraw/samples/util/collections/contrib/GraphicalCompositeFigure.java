
package org.jhotdraw.contrib; 
import java.awt.Graphics; 
import java.awt.Point; 
import java.awt.Rectangle; 
import java.util.List; 
import org.jhotdraw.figures.RectangleFigure; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.FigureAttributeConstant; 
import org.jhotdraw.framework.FigureChangeEvent; 
import org.jhotdraw.framework.HandleEnumeration; 
import org.jhotdraw.standard.BoxHandleKit; 
import org.jhotdraw.standard.CompositeFigure; 
import org.jhotdraw.standard.HandleEnumerator; 
import org.jhotdraw.util.CollectionsFactory; 
public  class  GraphicalCompositeFigure  extends CompositeFigure  implements Layoutable {
		private Figure	myPresentationFigure;

		private Layouter myLayouter;

		private static final long serialVersionUID = 1265742491024232713L;

		public GraphicalCompositeFigure() {	this(new RectangleFigure());	}

		public GraphicalCompositeFigure(Figure newPresentationFigure) {	super();	setPresentationFigure(newPresentationFigure);	initialize();	}

		protected void initialize() {	if (getLayouter() != null) {	setLayouter(getLayouter().create(this));	}	else {	setLayouter(new StandardLayouter(this));	}	}

		public Object clone() {	Object cloneObject = super.clone();	((GraphicalCompositeFigure)cloneObject).initialize();	return cloneObject;	}

		public Rectangle displayBox() {	return getPresentationFigure().displayBox();	}

		public void basicDisplayBox(Point origin, Point corner) {	Rectangle r = getLayouter().layout(origin, corner);	getPresentationFigure().basicDisplayBox(r.getLocation(),	new Point((int)r.getMaxX(), (int)r.getMaxY()));	}

		protected void basicMoveBy(int dx, int dy) {	super.basicMoveBy(dx, dy);	getPresentationFigure().moveBy(dx, dy);	}

		public void update() {	willChange();	layout();	change();	changed();	}

		public void draw(Graphics g) {	getPresentationFigure().draw(g);	super.draw(g);	}

		public HandleEnumeration handles() {	List handles = CollectionsFactory.current().createList();	BoxHandleKit.addHandles(this, handles);	return new HandleEnumerator(handles);	}

		public Object getAttribute(String name) {	if (getPresentationFigure() != null) {	return getPresentationFigure().getAttribute(name);	}	else {	return super.getAttribute(name);	}	}

		public Object getAttribute(FigureAttributeConstant attributeConstant) {	if (getPresentationFigure() != null) {	return getPresentationFigure().getAttribute(attributeConstant);	}	else {	return super.getAttribute(attributeConstant);	}	}

		public void setAttribute(String name, Object value) {	if (getPresentationFigure() != null) {	getPresentationFigure().setAttribute(name, value);	}	else {	super.setAttribute(name, value);	}	}

		public void setAttribute(FigureAttributeConstant attributeConstant, Object value) {	if (getPresentationFigure() != null) {	getPresentationFigure().setAttribute(attributeConstant, value);	}	else {	super.setAttribute(attributeConstant, value);	}	}

		public void setPresentationFigure(Figure newPresentationFigure) {	myPresentationFigure = newPresentationFigure;	}

		public Figure getPresentationFigure() {	return myPresentationFigure;	}

		public void layout() {	if (getLayouter() != null) {	Rectangle r = getLayouter().calculateLayout(displayBox().getLocation(), displayBox().getLocation());	displayBox(r.getLocation(), new Point(r.x + r.width, r.y + r.height));	}	}

		public void setLayouter(Layouter newLayouter) {	myLayouter = newLayouter;	}

		public Layouter getLayouter() {	return myLayouter;	}

		protected void change() {	if (listener() != null) {	listener().figureRequestUpdate(new FigureChangeEvent(this));	}	}

		public void figureRequestRemove(FigureChangeEvent e) {	if (listener() != null) {	if (includes(e.getFigure())) {	Rectangle r = invalidateRectangle(displayBox());	listener().figureRequestRemove(new FigureChangeEvent(this, r, e));	}	else {	super.figureRequestRemove(e);	}	}	}


}
