
package org.jhotdraw.standard; 
import java.awt.Graphics; 
import java.awt.Insets; 
import java.awt.Point; 
import java.awt.Rectangle; 
import org.jhotdraw.framework.ConnectionFigure; 
import org.jhotdraw.framework.Connector; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.FigureAttributeConstant; 
import org.jhotdraw.framework.FigureChangeEvent; 
import org.jhotdraw.framework.FigureChangeListener; 
import org.jhotdraw.framework.FigureEnumeration; 
import org.jhotdraw.framework.HandleEnumeration; 
import org.jhotdraw.framework.Locator; 
public abstract  class  DecoratorFigure 	extends AbstractFigure 	implements FigureChangeListener {
		private Figure myDecoratedFigure;

		private static final long serialVersionUID = 8993011151564573288L;

		private int decoratorFigureSerializedDataVersion = 1;

		public DecoratorFigure() {	initialize();	}

		public DecoratorFigure(Figure figure) {	initialize();	decorate(figure);	}

		protected void initialize() {	}

		public Insets connectionInsets() {	return getDecoratedFigure().connectionInsets();	}

		public boolean canConnect() {	return getDecoratedFigure().canConnect();	}

		public boolean containsPoint(int x, int y) {	return getDecoratedFigure().containsPoint(x, y);	}

		public void decorate(Figure figure) {	setDecoratedFigure(figure);	getDecoratedFigure().addToContainer(this);	}

		public Figure peelDecoration() {	getDecoratedFigure().removeFromContainer(this);	removeDependendFigure(getDecoratedFigure());	return getDecoratedFigure();	}

		public void setDecoratedFigure(Figure newDecoratedFigure) {	myDecoratedFigure = newDecoratedFigure;	}

		public Figure getDecoratedFigure() {	return myDecoratedFigure;	}

		public Rectangle displayBox() {	return getDecoratedFigure().displayBox();	}

		public void basicDisplayBox(Point origin, Point corner) {	getDecoratedFigure().basicDisplayBox(origin, corner);	}

		public void draw(Graphics g) {	getDecoratedFigure().draw(g);	}

		public Figure findFigureInside(int x, int y) {	Figure foundFigure = getDecoratedFigure().findFigureInside(x, y);	if ((foundFigure != null) && (foundFigure == getDecoratedFigure())) {	return this;	}	else {	return foundFigure;	}	}

		public HandleEnumeration handles() {	return getDecoratedFigure().handles();	}

		public boolean includes(Figure figure) {	return (super.includes(figure) || getDecoratedFigure().includes(figure));	}

		public void moveBy(int x, int y) {	getDecoratedFigure().moveBy(x, y);	}

		protected void basicMoveBy(int x, int y) {	}

		public void release() {	super.release();	getDecoratedFigure().removeFromContainer(this);	getDecoratedFigure().release();	}

		public void figureInvalidated(FigureChangeEvent e) {	if (listener() != null) {	listener().figureInvalidated(e);	}	}

		public void figureChanged(FigureChangeEvent e) {	}

		public void figureRemoved(FigureChangeEvent e) {	}

		public void figureRequestUpdate(FigureChangeEvent e) {	if (listener() != null) {	listener().figureRequestUpdate(e);	}	}

		public void figureRequestRemove(FigureChangeEvent e) {	if (listener() != null) {	listener().figureRequestRemove(new FigureChangeEvent(this));	}	}

		public FigureEnumeration figures() {	return getDecoratedFigure().figures();	}

		public FigureEnumeration decompose() {	return getDecoratedFigure().decompose();	}

		public void setAttribute(String name, Object value) {	getDecoratedFigure().setAttribute(name, value);	}

		public void setAttribute(FigureAttributeConstant attributeConstant, Object value) {	getDecoratedFigure().setAttribute(attributeConstant, value);	}

		public Object getAttribute(String name) {	return getDecoratedFigure().getAttribute(name);	}

		public Object getAttribute(FigureAttributeConstant attributeConstant) {	return getDecoratedFigure().getAttribute(attributeConstant);	}

		public Locator connectedTextLocator(Figure text) {	return getDecoratedFigure().connectedTextLocator(text);	}

		public Connector connectorAt(int x, int y) {	return getDecoratedFigure().connectorAt(x, y);	}

		public void connectorVisibility(boolean isVisible, ConnectionFigure courtingConnection) {	getDecoratedFigure().connectorVisibility(isVisible, null);	}

		public TextHolder getTextHolder() {	return getDecoratedFigure().getTextHolder();	}

		public synchronized FigureEnumeration getDependendFigures() {	return getDecoratedFigure().getDependendFigures();	}

		public synchronized void addDependendFigure(Figure newDependendFigure) {	getDecoratedFigure().addDependendFigure(newDependendFigure);	}

		public synchronized void removeDependendFigure(Figure oldDependendFigure) {	getDecoratedFigure().removeDependendFigure(oldDependendFigure);	}


}
