package org.jhotdraw.ccconcerns.tools.undo;

import org.jhotdraw.figures.ScribbleTool;
import org.jhotdraw.util.Undoable;

import org.jhotdraw.ccconcerns.commands.undo.PasteCommandUndo;

/**
 * Undo support for ScribbleTool - just a partial refactoring for now, 
 * needed because the move of UndoActivity from PasteCommand
 * to the undo support aspect.
 * 
 * @author Marius Marin
 */
public aspect ScribbleToolUndo {
	
	/**
	 * Factory method for undo activity
	 */
	/*@AJHD protected*/public Undoable ScribbleTool.createUndoActivity() {
		return new /*@AJHD PasteCommand*/PasteCommandUndo.UndoActivity(view());
	}


}
