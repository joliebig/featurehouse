
package org.jhotdraw.framework; 
import java.awt.*; 
import java.awt.image.ImageObserver; 
import java.util.Collection; 
public  interface  DrawingView  extends ImageObserver, DrawingChangeListener {
		public void setEditor(DrawingEditor editor);

		public Tool tool();

		public Drawing drawing();

		public void setDrawing(Drawing d);

		public DrawingEditor editor();

		public Figure add(Figure figure);

		public Figure remove(Figure figure);

		public void addAll(Collection figures);

		public Dimension getSize();

		public Dimension getMinimumSize();

		public Dimension getPreferredSize();

		public void setDisplayUpdate(Painter updateStrategy);

		public Painter getDisplayUpdate();

		public FigureEnumeration selection();

		public FigureEnumeration selectionZOrdered();

		public int selectionCount();

		public boolean isFigureSelected(Figure checkFigure);

		public void addToSelection(Figure figure);

		public void addToSelectionAll(Collection figures);

		public void addToSelectionAll(FigureEnumeration fe);

		public void removeFromSelection(Figure figure);

		public void toggleSelection(Figure figure);

		public void clearSelection();

		public FigureSelection getFigureSelection();

		public Handle findHandle(int x, int y);

		public Point lastClick();

		public void setConstrainer(PointConstrainer p);

		public PointConstrainer getConstrainer();

		public void checkDamage();

		public void repairDamage();

		public void paint(Graphics g);

		public Image createImage(int width, int height);

		public Graphics getGraphics();

		public Color getBackground();

		public void setBackground(Color c);

		public void drawAll(Graphics g);

		public void draw(Graphics g, FigureEnumeration fe);

		public void drawHandles(Graphics g);

		public void drawDrawing(Graphics g);

		public void drawBackground(Graphics g);

		public void setCursor(Cursor c);

		public void freezeView();

		public void unfreezeView();

		public FigureEnumeration getConnectionFigures(Figure inFigure);

		public FigureEnumeration insertFigures(FigureEnumeration inFigures, int dx, int dy, boolean bCheck);

		public boolean isInteractive();


}
