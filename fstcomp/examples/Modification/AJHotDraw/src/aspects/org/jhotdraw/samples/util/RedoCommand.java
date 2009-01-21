
package org.jhotdraw.util; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.framework.*; 
public  class  RedoCommand  extends AbstractCommand {
		public RedoCommand(String name, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	}

		public void execute() {	UndoManager um = getDrawingEditor().getUndoManager();	if ((um == null) || !um.isRedoable()) {	return;	}	Undoable lastRedoable = um.popRedo();	boolean hasBeenUndone = lastRedoable.redo();	if (hasBeenUndone && lastRedoable.isUndoable()) {	um.pushUndo(lastRedoable);	}	lastRedoable.getDrawingView().checkDamage();	getDrawingEditor().figureSelectionChanged(lastRedoable.getDrawingView());	}

		public boolean isExecutableWithView() {	UndoManager um = getDrawingEditor().getUndoManager();	if ((um != null) && (um.getRedoSize() > 0)) {	return true;	} return false;	}


}
