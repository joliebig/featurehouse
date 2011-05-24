package org.jhotdraw.ccconcerns.commands.undo;

import org.jhotdraw.standard.DeleteCommand;
import org.jhotdraw.standard.FigureTransferCommand;
import org.jhotdraw.util.Undoable;
import org.jhotdraw.util.UndoableAdapter;

/**
 * Undo support for DeleteCommand. Some of the more general concerns
 * (ie, those that cover more Command elements) are in CommandUndo.
 * 
 * @author Marius Marin
 */
public privileged aspect DeleteCommandUndo {

	/**
	 * Factory method for undo activity
	 * @return Undoable
	 */
//	/*@AJHD protected*/public Undoable DeleteCommand.createUndoActivity() {
//		return new /*@ AJHD DeleteCommand*/DeleteCommandUndo.UndoActivity(this);
//	}
	
	
//	//@see AlignCommandUndo 
//    protected pointcut commandExecuteInitUndo(DeleteCommand acommand) :
//		this(acommand)
//		&& execution(void DeleteCommand.execute())
//		&& within(DeleteCommand);
//
//
//
//	//@see AlignCommandUndo
//    before(DeleteCommand acommand) : commandExecuteInitUndo(acommand) {
//		acommand.setUndoActivity(acommand.createUndoActivity());
//	}
//
//	//@see AlignCommandUndo
//   before(DeleteCommand acommand) : commandExecuteInitUndo(acommand) {
//		acommand.getUndoActivity().setAffectedFigures(acommand.view().selection());
//	}
//
//
//    //---------
    
//	public static class UndoActivity extends UndoableAdapter {
//
//		private FigureTransferCommand myCommand;
//
//		/**
//		 * Constructor for <code>UndoActivity</code>.
//		 * @param newCommand parent command
//		 */
//		public UndoActivity(FigureTransferCommand newCommand) {
//			super(newCommand.view());
//			myCommand = newCommand;
//			setUndoable(true);
//			setRedoable(true);
//		}
//
//		/**
//		 * @see org.jhotdraw.util.Undoable#undo()
//		 */
//		public boolean undo() {
//			if (super.undo() && getAffectedFigures().hasNextFigure()) {
//				getDrawingView().clearSelection();
//				setAffectedFigures(
//					myCommand.insertFigures(getAffectedFiguresReversed(), 0, 0));
//				return true;
//			}
//			return false;
//		}
//
//		/**
//		 * @see org.jhotdraw.util.Undoable#redo()
//		 */
//		public boolean redo() {
//			// do not call execute directly as the selection might has changed
//			if (isRedoable()) {
//				myCommand.deleteFigures(getAffectedFigures());
//				getDrawingView().clearSelection();
//				return true;
//			}
//			return false;
//		}
//	}

}


