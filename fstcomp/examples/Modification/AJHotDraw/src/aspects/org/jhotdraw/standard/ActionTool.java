
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import java.awt.event.MouseEvent; 
public abstract  class  ActionTool  extends AbstractTool {
		public ActionTool(DrawingEditor newDrawingEditor) {	super(newDrawingEditor);	}

		public void mouseDown(MouseEvent e, int x, int y) {	super.mouseDown(e,x,y);	Figure target = drawing().findFigure(x, y);	if (target != null) {	view().addToSelection(target);	action(target);	}	}

		public void mouseUp(MouseEvent e, int x, int y) {	editor().toolDone();	}

		public abstract void action(Figure figure);


}
