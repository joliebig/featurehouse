
package org.jhotdraw.standard; 
import java.awt.Point; 
import org.jhotdraw.framework.*; 
public  class  ToggleGridCommand  extends AbstractCommand {
		private Point fGrid;

		public ToggleGridCommand(String name, DrawingEditor newDrawingEditor, Point grid) {	super(name, newDrawingEditor);	fGrid = new Point(grid.x, grid.y);	}

		public void execute() {	PointConstrainer grid = view().getConstrainer();	if (grid != null) {	view().setConstrainer(null);	}	else {	view().setConstrainer(new GridConstrainer(fGrid.x, fGrid.y));	}	}


}
