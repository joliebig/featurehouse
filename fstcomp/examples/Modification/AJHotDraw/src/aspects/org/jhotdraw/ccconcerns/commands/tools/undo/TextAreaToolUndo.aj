package org.jhotdraw.ccconcerns.tools.undo;

import org.jhotdraw.contrib.TextAreaTool;
import org.jhotdraw.util.Undoable;

/**
 * Undo support for TextAreaTool - see that this is just a partial refactoring
 * for now - it was needed because moving the UndoActivity from DeleteCommand
 * to the undo support aspect caused compilation errors in TextTool.
 * 
 * @author Marius Marin
 */
public privileged aspect TextAreaToolUndo {

	/**
	 * Factory method for undo activity
	 *
	 * @return   Description of the Return Value
	 */
	/*@AJHD protected*/public Undoable TextAreaTool.createUndoActivity() {
		return new TextAreaTool.UndoActivity(view(), getTypingTarget().getText());
	}

	
}

