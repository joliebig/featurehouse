
package org.jhotdraw.contrib; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.contrib.zoom.ZoomDrawingView; 
import org.jhotdraw.standard.*; 
import javax.swing.JPopupMenu; 
import java.awt.*; 
import java.awt.event.*; 
public  class  CustomSelectionTool  extends SelectionTool {
		public CustomSelectionTool(DrawingEditor editor) {	super( editor );	}

		public void mouseDown(MouseEvent e, int x, int y) {	setView((DrawingView)e.getSource());	if (e.isPopupTrigger()) {	handlePopupMenu(e, x, y);	}	else {	super.mouseDown(e, x, y);	handleMouseDown(e, x, y);	}	}

		public void mouseDrag(MouseEvent e, int x, int y) {	if (!e.isPopupTrigger()) {	super.mouseDrag(e, x, y);	}	}

		public void mouseUp(MouseEvent e, int x, int y) {	if (e.isPopupTrigger()) {	handlePopupMenu(e, x, y);	super.mouseUp(e, x, y);	}	else if (e.getClickCount() == 2) {	super.mouseUp(e, x, y);	handleMouseDoubleClick(e, x, y);	}	else {	super.mouseUp(e, x, y);	handleMouseUp(e, x, y);	handleMouseClick(e, x, y);	}	}

		protected void handleMouseDown(MouseEvent e, int x, int y) {	}

		protected void handleMouseUp(MouseEvent e, int x, int y) {	}

		protected void handleMouseClick(MouseEvent e, int x, int y) {	}

		protected void handleMouseDoubleClick(MouseEvent e, int x, int y) {	}

		protected void handlePopupMenu(MouseEvent e, int x, int y) {	Figure figure = drawing().findFigure(e.getX(), e.getY());	if (figure != null) {	Object attribute = figure.getAttribute(FigureAttributeConstant.POPUP_MENU);	if (attribute == null) {	figure = drawing().findFigureInside(e.getX(), e.getY());	}	if (figure != null) {	showPopupMenu(figure, e.getX(), e.getY(), e.getComponent());	}	}	}

		protected void showPopupMenu(Figure figure, int x, int y, Component comp) {	Object attribute = figure.getAttribute(FigureAttributeConstant.POPUP_MENU);	if ((attribute != null) && (attribute instanceof JPopupMenu)) {	JPopupMenu popup = (JPopupMenu)attribute;	if (popup instanceof PopupMenuFigureSelection) {	((PopupMenuFigureSelection)popup).setSelectedFigure(figure);	}	Point newLocation;	try {	newLocation = comp.getLocationOnScreen();	} catch (IllegalComponentStateException e) {	return;	}	if (comp instanceof ZoomDrawingView) {	double scale = ((ZoomDrawingView) comp).getScale();	x *= scale;	y *= scale;	}	newLocation.translate(x,y);	popup.setLocation(newLocation);	popup.setInvoker(comp);	popup.setVisible(true);	}	}


}
