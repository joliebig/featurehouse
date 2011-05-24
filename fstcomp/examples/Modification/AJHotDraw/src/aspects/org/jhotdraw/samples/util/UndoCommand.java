
package org.jhotdraw.util; 
import org.jhotdraw.standard.*; 
import org.jhotdraw.framework.*; 
public  class  UndoCommand  extends AbstractCommand {
		public UndoCommand(String name, DrawingEditor newDrawingEditor) {	super(name, newDrawingEditor);	}

		public void execute() {	UndoManager um = getDrawingEditor().getUndoManager();	if ((um == null) || !um.isUndoable()) {	return;	}	Undoable lastUndoable = um.popUndo();	boolean hasBeenUndone = lastUndoable.undo();	if (hasBeenUndone && lastUndoable.isRedoable()) {	um.pushRedo(lastUndoable);	}	lastUndoable.getDrawingView().checkDamage();	getDrawingEditor().figureSelectionChanged(lastUndoable.getDrawingView());	}

		public boolean isExecutableWithView() {	UndoManager um = getDrawingEditor().getUndoManager();	if ((um != null) && (um.getUndoSize() > 0)) {	return true;	}	return false;	}


}
