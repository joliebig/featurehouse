package org.jhotdraw.ccconcerns.tools.undo;

import org.jhotdraw.standard.CreationTool;
import org.jhotdraw.util.Undoable;

import org.jhotdraw.ccconcerns.commands.undo.PasteCommandUndo;

/**
 * Undo support for CreationTool - just a partial refactoring for now, 
 * needed because the move of UndoActivity from PasteCommand
 * to the undo support aspect.
 * 
 * @author Marius Marin
 */
public aspect CreationToolUndo {

	/**
	 * Factory method for undo activity
	 */
	/*@AJHD protected*/public Undoable CreationTool.createUndoActivity() {
		return new /*@AJHD PasteCommand*/PasteCommandUndo.UndoActivity(getActiveView());
	}

}
