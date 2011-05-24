package org.jhotdraw.ccconcerns.commands.undo;

import java.util.List;

import org.jhotdraw.framework.FigureEnumeration;
import org.jhotdraw.standard.CutCommand;
import org.jhotdraw.standard.FigureEnumerator;
import org.jhotdraw.standard.FigureTransferCommand;
import org.jhotdraw.util.CollectionsFactory;
import org.jhotdraw.util.Undoable;
import org.jhotdraw.util.UndoableAdapter;

/**
 * Undo support for CutCommand. Some of the more general concerns
 * (ie, those that cover more Command elements) are in CommandUndo.
 * 
 * @author Marius Marin
 * 
 */
public privileged aspect CutCommandUndo {

	/**
	 * Factory method for undo activity
	 * @return Undoable
	 */
	/*@AJHD protected*/public Undoable CutCommand.createUndoActivity() {
		return new /*@AJHD CutCommand*/CutCommandUndo.UndoActivity(this);
	}
	
	//@see AlignCommandUndo 
    pointcut commandExecuteInitUndo(CutCommand acommand) :
		this(acommand)
		&& execution(void CutCommand.execute())
		&& within(CutCommand);



	//@see AlignCommandUndo
    before(CutCommand acommand) : commandExecuteInitUndo(acommand) {
		acommand.setUndoActivity(acommand.createUndoActivity());
	}
    
    //This sets both the affected figures and the selected ones as for the original code; however,
    //it does not connect to the execution of the Command, but to a call in this execution whose
    //result is needed here. This kind of advice does not really reflect my goal of advising the execution.
//    after(CutCommand acommand) returning(FigureEnumeration collectedAffectedFigures): 
//    	call(FigureEnumeration CutCommand.collectAffectedFigures()) &&
//    	withincode(void CutCommand.execute()) &&
//    	this(acommand){
//    		acommand.getUndoActivity().setAffectedFigures(collectedAffectedFigures);
//    		UndoActivity ua = (UndoActivity) acommand.getUndoActivity();
//    		ua.setSelectedFigures(acommand.view().selection());
//    }


    //See the comments for the previous advice - the one commented out
    before(CutCommand acommand) : commandExecuteInitUndo(acommand) {
		acommand.getUndoActivity().setAffectedFigures(acommand.collectAffectedFigures());
		UndoActivity ua = (UndoActivity) acommand.getUndoActivity();
		ua.setSelectedFigures(acommand.view().selection());

	}

	
	public static class UndoActivity extends UndoableAdapter {

		private FigureTransferCommand myCommand;
		private List mySelectedFigures;

		/**
		 * Constructor for <code>UndoActivity</code>.
		 * @param newCommand
		 */
		public UndoActivity(FigureTransferCommand newCommand) {
			super(newCommand.view());
			myCommand = newCommand;
			setUndoable(true);
			setRedoable(true);
		}

		/**
		 * @see org.jhotdraw.util.Undoable#undo()
		 */
		public boolean undo() {
			if (super.undo() && getAffectedFigures().hasNextFigure()) {
				getDrawingView().clearSelection();
				myCommand.insertFigures(getAffectedFiguresReversed(), 0, 0);
				return true;
			}
			return false;
		}

		/**
		 * @see org.jhotdraw.util.Undoable#redo()
		 */
		public boolean redo() {
			// do not call execute directly as the selection might has changed
			if (isRedoable()) {
				myCommand.copyFigures(getSelectedFigures(), getSelectedFiguresCount());
				myCommand.deleteFigures(getAffectedFigures());
				return true;
			}

			return false;
		}

		/**
		 * Preserve the selection of figures the moment the command was executed.
		 * @param newSelectedFigures
		 */
		public void setSelectedFigures(FigureEnumeration newSelectedFigures) {
			// the enumeration is not reusable therefore a copy is made
			// to be able to undo-redo the command several time
			rememberSelectedFigures(newSelectedFigures);
		}

		/**
		 * Preserve a copy of the enumeration in a private list.
		 * @param toBeRemembered
		 */
		protected void rememberSelectedFigures(FigureEnumeration toBeRemembered) {
			mySelectedFigures = CollectionsFactory.current().createList();
			while (toBeRemembered.hasNextFigure()) {
				mySelectedFigures.add(toBeRemembered.nextFigure());
			}
		}
	
		/**
		 * Returns the selection of figures to perform the command on.
		 * @return
		 */
		public FigureEnumeration getSelectedFigures() {
			return new FigureEnumerator(
				CollectionsFactory.current().createList(mySelectedFigures));
		}

		/**
		 * Returns the size of the selection.
		 * @return
		 */
		public int getSelectedFiguresCount() {
			return mySelectedFigures.size();
		}

		/**
		 * @see org.jhotdraw.util.UndoableAdapter#release()
		 */
		public void release() {
			super.release();
			FigureEnumeration fe = getSelectedFigures();
			while (fe.hasNextFigure()) {
				fe.nextFigure().release();
			}
			setSelectedFigures(FigureEnumerator.getEmptyEnumeration());
		}
	}


}
