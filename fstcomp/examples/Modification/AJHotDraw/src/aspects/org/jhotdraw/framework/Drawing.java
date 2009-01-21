
package org.jhotdraw.framework; 
import java.awt.Graphics; 
import java.awt.Rectangle; 
import java.io.Serializable; 
import java.util.Iterator; 
import java.util.List; 
public  interface  Drawing 	extends FigureChangeListener, Serializable {
		public void release();

		public FigureEnumeration figures();

		public FigureEnumeration figures(Rectangle viewRectangle);

		public FigureEnumeration figuresReverse();

		public Figure findFigure(int x, int y);

		public Figure findFigure(Rectangle r);

		public Figure findFigureWithout(int x, int y, Figure without);

		public Figure findFigure(Rectangle r, Figure without);

		public Figure findFigureInside(int x, int y);

		public Figure findFigureInsideWithout(int x, int y, Figure without);

		public boolean includes(Figure figure);

		public boolean containsFigure(Figure figure);

		public void addDrawingChangeListener(DrawingChangeListener listener);

		public void removeDrawingChangeListener(DrawingChangeListener listener);

		public Iterator drawingChangeListeners();

		public Figure add(Figure figure);

		public void addAll(List newFigures);

		public void addAll(FigureEnumeration fe);

		public Figure remove(Figure figure);

		public Figure orphan(Figure figure);

		public void orphanAll(List orphanFigures);

		public void orphanAll(FigureEnumeration fe);

		public void removeAll(List figures);

		public void removeAll(FigureEnumeration fe);

		public Figure replace(Figure figure, Figure replacement);

		public void sendToBack(Figure figure);

		public void bringToFront(Figure figure);

		public void sendToLayer(Figure figure, int layerNr);

		public int getLayer(Figure figure);

		public Figure getFigureFromLayer(int layerNr);

		public void draw(Graphics g);

		public void draw(Graphics g, FigureEnumeration fe);

		public void lock();

		public void unlock();

		public void init(Rectangle viewRectangle);

		public String getTitle();

		public void setTitle(String name);


}
