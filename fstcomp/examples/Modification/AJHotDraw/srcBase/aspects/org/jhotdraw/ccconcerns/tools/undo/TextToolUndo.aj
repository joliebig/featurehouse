package org.jhotdraw.ccconcerns.tools.undo;

import org.jhotdraw.figures.TextTool;
import org.jhotdraw.figures.ConnectedTextTool;
import org.jhotdraw.standard.DeleteCommand;
import org.jhotdraw.standard.FigureTransferCommand;
import org.jhotdraw.util.Undoable;

import org.jhotdraw.ccconcerns.commands.undo.DeleteCommandUndo;
import org.jhotdraw.ccconcerns.commands.undo.PasteCommandUndo;

/**
 * Undo support for TextTool - see that this is just a partial refactoring
 * for now - it was needed because moving the UndoActivity from DeleteCommand
 * to the undo support aspect caused compilation errors in TextTool.
 * 
 * @author Marius Marin
 */
public privileged aspect TextToolUndo {

    declare parents: ConnectedTextTool.DeleteUndoActivity extends DeleteCommandUndo.UndoActivity;
	
	/*@AJHD protected*/public Undoable TextTool.createDeleteUndoActivity() {
		FigureTransferCommand cmd = new DeleteCommand("Delete", editor());
		return new /*@AJHD DeleteCommand*/DeleteCommandUndo.UndoActivity(cmd);
	}

	/*@AJHD protected*/public Undoable ConnectedTextTool.createDeleteUndoActivity() {
		FigureTransferCommand cmd = new DeleteCommand("Delete", editor());
		return new DeleteUndoActivity(cmd, getConnectedFigure());
	}

	/*@AJHD protected*/public Undoable TextTool.createPasteUndoActivity() {
		return new /*@AJHD PasteCommand*/PasteCommandUndo.UndoActivity(view());
	}
	
	/**
	 * Factory method for undo activity
	 */
	/*@AJHD protected*/public Undoable TextTool.createUndoActivity() {
		return new TextTool.UndoActivity(view(), getTypingTarget().getText());
	}

	/**
	 * Factory method for undo activity
	 */
	/*@AJHD protected*/public Undoable ConnectedTextTool.createUndoActivity() {
		return new ConnectedTextTool.UndoActivity(view(), getTypingTarget().getText());
	}

}
