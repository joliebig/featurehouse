
package org.jhotdraw.standard; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.FigureEnumeration; 
public  class  DeleteCommand  extends FigureTransferCommand {
		public DeleteCommand(String name, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	}

		public void execute() {	FigureEnumeration fe = collectAffectedFigures();	deleteFigures(fe);	}

		protected boolean isExecutableWithView() {	return view().selectionCount() > 0;	}


}
