
package org.jhotdraw.standard; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.FigureEnumeration; 
public  class  BringToFrontCommand  extends AbstractCommand {
		public BringToFrontCommand(String name, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	}

		public void execute() {	FigureEnumeration fe = view().selection();	while (fe.hasNextFigure()) {	view().drawing().bringToFront(fe.nextFigure());	}	}

		public boolean isExecutableWithView() {	return view().selectionCount() > 0;	}


}
