package org.jhotdraw.ccconcerns.handles.undo;

import org.jhotdraw.framework.DrawingView;
import org.jhotdraw.standard.ConnectionHandle;
import org.jhotdraw.util.Undoable;

import org.jhotdraw.ccconcerns.commands.undo.PasteCommandUndo;

/**
 * Undo support for ConnectionHandle - just a partial refactoring for now, 
 * needed because the move of UndoActivity from PasteCommand
 * to the undo support aspect.
 * 
 * @author Marius Marin
 */
public aspect ConnectionHandleUndo {

	/**
	 * Factory method for undo activity.
	 */
	/*@AJHD protected*/public Undoable ConnectionHandle.createUndoActivity(DrawingView view) {
		return new /*@AJHD PasteCommand*/PasteCommandUndo.UndoActivity(view);
	}

}
