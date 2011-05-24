
package org.jhotdraw.figures; 
import org.jhotdraw.framework.DrawingEditor; 
import org.jhotdraw.framework.Figure; 
import org.jhotdraw.framework.FigureEnumeration; 
import org.jhotdraw.standard.AbstractCommand; 
public  class  GroupCommand  extends AbstractCommand {
		public GroupCommand(String name, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	}

		public void execute() {	groupFigures();	}

		public void groupFigures() {	FigureEnumeration selection = view().selection();	view().drawing().orphanAll(selection);	view().clearSelection();	selection.reset();	GroupFigure group = new GroupFigure();	group.addAll(selection);	Figure figure = view().drawing().add(group);	view().addToSelection(figure);	}

		public boolean isExecutableWithView() {	return view().selectionCount() > 1;	}


}
