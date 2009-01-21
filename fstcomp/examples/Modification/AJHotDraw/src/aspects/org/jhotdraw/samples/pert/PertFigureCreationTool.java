
package org.jhotdraw.samples.pert; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
public  class  PertFigureCreationTool  extends CreationTool {
		public PertFigureCreationTool(DrawingEditor newDrawingEditor) {	super(newDrawingEditor);	}

		protected Figure createFigure() {	return new PertFigure();	}


}
