
package org.jhotdraw.contrib.zoom; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.Tool; 
import org.jhotdraw.standard.AbstractTool; 
import java.awt.event.InputEvent; 
import java.awt.event.MouseEvent; 
public  class  ZoomTool  extends AbstractTool {
		private Tool child;

		public ZoomTool(DrawingEditor editor) {	super(editor);	}

		public void mouseDown(MouseEvent e, int x, int y) {	super.mouseDown(e,x,y);	if ((e.getModifiers() & InputEvent.BUTTON1_MASK) != 0) {	if (child != null) {	return;	}	view().freezeView();	child = new ZoomAreaTracker(editor());	child.mouseDown(e, x, y);	}	else if ((e.getModifiers() & InputEvent.BUTTON2_MASK) != 0) {	((ZoomDrawingView) view()).deZoom(x, y);	}	else if ((e.getModifiers() & InputEvent.BUTTON3_MASK) != 0) {	if ((e.getModifiers() & InputEvent.SHIFT_MASK) != 0) {	((ZoomDrawingView)view()).zoomIn(x, y);	}	else if ((e.getModifiers() & InputEvent.CTRL_MASK) != 0) {	((ZoomDrawingView) view()).deZoom(x, y);	}	else {	((ZoomDrawingView)view()).zoomOut(x, y);	}	}	}

		public void mouseDrag(MouseEvent e, int x, int y) {	if (child != null) {	child.mouseDrag(e, x, y);	}	}

		public void mouseUp(MouseEvent e, int x, int y) {	if (child != null) {	view().unfreezeView();	child.mouseUp(e, x, y);	}	child = null;	}


}
