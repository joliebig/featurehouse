
package org.jhotdraw.samples.javadraw; 
import java.awt.event.MouseEvent; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
public  class  MySelectionTool  extends SelectionTool {
		public MySelectionTool(DrawingEditor newDrawingEditor) {	super(newDrawingEditor);	}

		public void mouseDown(MouseEvent e, int x, int y) {	setView((DrawingView)e.getSource());	if (e.getClickCount() == 2) {	Figure figure = drawing().findFigure(e.getX(), e.getY());	if (figure != null) {	inspectFigure(figure);	return;	}	}	super.mouseDown(e, x, y);	}

		protected void inspectFigure(Figure f) {	System.out.println("inspect figure"+f);	}


}
