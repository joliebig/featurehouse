
package org.jhotdraw.standard; 
import java.util.List; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.FigureEnumeration; 
import org.jhotdraw.util.CollectionsFactory; 
public  class  CutCommand  extends FigureTransferCommand {
		public CutCommand(String name, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	}

		public void execute() {	FigureEnumeration fe = collectAffectedFigures();	FigureEnumeration selection = view().selection();	List l = CollectionsFactory.current().createList();	while(selection.hasNextFigure()) {	l.add(selection.nextFigure());	}	selection.reset();	copyFigures(selection, l.size());	deleteFigures(fe);	}

		public boolean isExecutableWithView() {	return view().selectionCount() > 0;	}


}
