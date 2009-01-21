
package org.jhotdraw.framework; 
import java.awt.Dimension; 
import java.awt.Graphics; 
import java.awt.Insets; 
import java.awt.Point; 
import java.awt.Rectangle; 
import java.io.Serializable; 
import org.jhotdraw.standard.TextHolder; 
public  interface  Figure 	extends Cloneable, Serializable {
		public static String POPUP_MENU = "POPUP_MENU";

		public void moveBy(int dx, int dy);

		public void basicDisplayBox(Point origin, Point corner);

		public void displayBox(Point origin, Point corner);

		public Rectangle displayBox();

		public void draw(Graphics g);

		public HandleEnumeration handles();

		public Dimension size();

		public Point center();

		public boolean isEmpty();

		public FigureEnumeration figures();

		public Figure findFigureInside(int x, int y);

		public boolean containsPoint(int x, int y);

		public Object clone();

		public void displayBox(Rectangle r);

		public boolean includes(Figure figure);

		public FigureEnumeration decompose();

		public void addToContainer(FigureChangeListener c);

		public void removeFromContainer(FigureChangeListener c);

		public void addDependendFigure(Figure newDependendFigure);

		public void removeDependendFigure(Figure oldDependendFigure);

		public FigureEnumeration getDependendFigures();

		public FigureChangeListener listener();

		public void addFigureChangeListener(FigureChangeListener l);

		public void removeFigureChangeListener(FigureChangeListener l);

		public void release();

		public void invalidate();

		public void willChange();

		public void changed();

		public boolean canConnect();

		public Connector connectorAt(int x, int y);

		public void connectorVisibility(boolean isVisible, ConnectionFigure connection);

		public Insets connectionInsets();

		public Locator connectedTextLocator(Figure text);

		public Object getAttribute(String name);

		public Object getAttribute(FigureAttributeConstant attributeConstant);

		public void setAttribute(String name, Object value);

		public void setAttribute(FigureAttributeConstant attributeConstant, Object value);

		public int getZValue();

		public void setZValue(int z);

		public void visit(FigureVisitor visitor);

		public TextHolder getTextHolder();

		public Figure getDecoratedFigure();


}
