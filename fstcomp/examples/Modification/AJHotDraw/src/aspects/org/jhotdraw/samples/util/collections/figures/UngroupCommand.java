
package org.jhotdraw.figures; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.util.UndoableAdapter; 
import org.jhotdraw.util.Undoable; 
public  class  UngroupCommand  extends AbstractCommand {
		public UngroupCommand(String name, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	}

		public void execute() {	setUndoActivity(createUndoActivity());	getUndoActivity().setAffectedFigures(view().selection());	view().clearSelection();	((UngroupCommand.UndoActivity)getUndoActivity()).ungroupFigures();	}

		public boolean isExecutableWithView() {	FigureEnumeration fe = view().selection();	while (fe.hasNextFigure()) {	Figure currentFigure = fe.nextFigure();	currentFigure = currentFigure.getDecoratedFigure();	if (!(currentFigure instanceof GroupFigure)) {	return false;	}	}	return view().selectionCount() > 0;	}

		protected Undoable createUndoActivity() {	return new UngroupCommand.UndoActivity(view());	}

		public static  class  UndoActivity  extends UndoableAdapter {
			public UndoActivity(DrawingView newDrawingView) {	super(newDrawingView);	setUndoable(true);	setRedoable(true);	}

			public boolean undo() {	if (!super.undo()) {	return false;	}	getDrawingView().clearSelection();	FigureEnumeration groupFigures = getAffectedFigures();	while (groupFigures.hasNextFigure()) {	Figure groupFigure = groupFigures.nextFigure();	getDrawingView().drawing().orphanAll(groupFigure.figures());	Figure figure = getDrawingView().drawing().add(groupFigure);	getDrawingView().addToSelection(figure);	}	return true;	}

			public boolean redo() {	if (isRedoable()) {	getDrawingView().drawing().orphanAll(getAffectedFigures());	getDrawingView().clearSelection();	ungroupFigures();	return true;	}	return false;	}

			protected void ungroupFigures() {	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	Figure selected = fe.nextFigure();	Figure group = getDrawingView().drawing().orphan(selected);	getDrawingView().drawing().addAll(group.figures());	getDrawingView().addToSelectionAll(group.figures());	}	}


	}


}
