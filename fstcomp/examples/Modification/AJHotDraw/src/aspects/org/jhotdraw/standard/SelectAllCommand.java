
package org.jhotdraw.standard; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.util.UndoableAdapter; 
import org.jhotdraw.util.Undoable; 
public  class  SelectAllCommand  extends AbstractCommand {
		public SelectAllCommand(String name, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	}

		public void execute() {	setUndoActivity(createUndoActivity());	getUndoActivity().setAffectedFigures(view().selection());	view().addToSelectionAll(view().drawing().figures());	}

		public boolean isExecutableWithView() {	FigureEnumeration fe = view().drawing().figures();	if (fe.hasNextFigure() && (fe.nextFigure() != null)) {	return true;	}	return false;	}

		protected Undoable createUndoActivity() {	return new SelectAllCommand.UndoActivity(view());	}

		public static  class  UndoActivity  extends UndoableAdapter {
			public UndoActivity(DrawingView newDrawingView) {	super(newDrawingView);	setUndoable(true);	setRedoable(true);	}

			public boolean undo() {	if (!super.undo()) {	return false;	}	getDrawingView().clearSelection();	getDrawingView().addToSelectionAll(getAffectedFigures()); return true;	}

			public boolean redo() {	if (isRedoable()) {	getDrawingView().addToSelectionAll(getDrawingView().drawing().figures());	return true;	}	return false;	}


	}


}
