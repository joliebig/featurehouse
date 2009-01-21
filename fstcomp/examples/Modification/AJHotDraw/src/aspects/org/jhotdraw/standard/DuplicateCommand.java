
package org.jhotdraw.standard; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.FigureEnumeration; 
import org.jhotdraw.framework.FigureSelection; 
public  class  DuplicateCommand  extends FigureTransferCommand {
		public DuplicateCommand(String name, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	}

		public void execute() {	FigureSelection selection = view().getFigureSelection();	FigureEnumeration figures = (FigureEnumeration)selection.getData(StandardFigureSelection.TYPE);	view().clearSelection();	insertFigures(figures, 10, 10);	}

		protected boolean isExecutableWithView() {	return view().selectionCount() > 0;	}


}
