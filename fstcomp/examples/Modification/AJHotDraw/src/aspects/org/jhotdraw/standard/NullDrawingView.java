
package org.jhotdraw.standard; 
import java.awt.*; 
import java.util.Collection; 
import java.util.Hashtable; 
import javax.swing.JPanel; 
import org.jhotdraw.framework.*; 
public  class  NullDrawingView  extends JPanel  implements DrawingView {
		private DrawingEditor myDrawingEditor;

		private Drawing myDrawing;

		private Painter myUpdateStrategy;

		private Color myBackgroundColor;

		private static Hashtable drawingViewManager = new Hashtable();

		protected NullDrawingView(DrawingEditor editor) {	setEditor(editor);	setDrawing(new StandardDrawing());	}

		public void setEditor(DrawingEditor editor) {	myDrawingEditor = editor;	}

		public Tool tool() {	return editor().tool();	}

		public Drawing drawing() {	return myDrawing;	}

		public void setDrawing(Drawing d) {	myDrawing = d;	}

		public DrawingEditor editor() {	return myDrawingEditor;	}

		public Figure add(Figure figure) {	return figure;	}

		public Figure remove(Figure figure) {	return figure;	}

		public void addAll(Collection figures) {	}

		public Dimension getSize() {	return new Dimension();	}

		public Dimension getMinimumSize() {	return new Dimension();	}

		public Dimension getPreferredSize() {	return new Dimension();	}

		public void setDisplayUpdate(Painter newUpdateStrategy) {	myUpdateStrategy = newUpdateStrategy;	}

		public Painter getDisplayUpdate() {	return myUpdateStrategy;	}

		public FigureEnumeration selection() {	return FigureEnumerator.getEmptyEnumeration();	}

		public FigureEnumeration selectionZOrdered() {	return FigureEnumerator.getEmptyEnumeration();	}

		public int selectionCount() {	return 0;	}

		public boolean isFigureSelected(Figure checkFigure) {	return false;	}

		public void addToSelection(Figure figure) {	}

		public void addToSelectionAll(Collection figures) {	}

		public void addToSelectionAll(FigureEnumeration fe) {	}

		public void removeFromSelection(Figure figure) {	}

		public void toggleSelection(Figure figure) {	}

		public void clearSelection() {	}

		public FigureSelection getFigureSelection() {	return new StandardFigureSelection(selection(), 0);	}

		public Handle findHandle(int x, int y) {	return null;	}

		public Point lastClick() {	return new Point();	}

		public void setConstrainer(PointConstrainer p) {	}

		public PointConstrainer getConstrainer() {	return null;	}

		public void checkDamage() {	}

		public void repairDamage() {	}

		public void paint(Graphics g) {	}

		public Image createImage(int width, int height) {	return null;	}

		public Graphics getGraphics() {	return null;	}

		public Color getBackground() {	return myBackgroundColor;	}

		public void setBackground(Color c) {	myBackgroundColor = c;	}

		public void drawAll(Graphics g) {	}

		public void draw(Graphics g, FigureEnumeration fe) {	}

		public void drawHandles(Graphics g) {	}

		public void drawDrawing(Graphics g) {	}

		public void drawBackground(Graphics g) {	}

		public void setCursor(org.jhotdraw.framework.Cursor c) {	}

		public void freezeView() {	}

		public void unfreezeView() {	}

		public FigureEnumeration getConnectionFigures(Figure inFigure) {	return FigureEnumerator.getEmptyEnumeration();	}

		public FigureEnumeration insertFigures(FigureEnumeration inFigures, int dx, int dy, boolean bCheck) {	return FigureEnumerator.getEmptyEnumeration();	}

		public void drawingInvalidated(DrawingChangeEvent e) {	}

		public void drawingRequestUpdate(DrawingChangeEvent e) {	}

		public void drawingTitleChanged(DrawingChangeEvent e) {	}

		public boolean isInteractive() {	return false;	}

		public synchronized static DrawingView getManagedDrawingView(DrawingEditor editor) {	if (drawingViewManager.containsKey(editor)) {	return (DrawingView)drawingViewManager.get(editor);	}	else {	DrawingView newDrawingView = new NullDrawingView(editor);	drawingViewManager.put(editor, newDrawingView);	return newDrawingView;	}	}


}
