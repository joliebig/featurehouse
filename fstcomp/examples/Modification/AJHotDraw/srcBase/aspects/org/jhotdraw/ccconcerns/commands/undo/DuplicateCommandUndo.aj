package org.jhotdraw.ccconcerns.commands.undo;

import org.jhotdraw.framework.FigureEnumeration;
import org.jhotdraw.standard.DuplicateCommand;
import org.jhotdraw.util.Undoable;

/**
 * Undo support for DuplicateCommand. Some of the more general concerns
 * (ie, those that cover more Command elements) are in CommandUndo.
 * 
 * @author Marius Marin
 */
public aspect DuplicateCommandUndo {

	/**
	 * Factory method for undo activity
	 */
	/*@AJHD protected*/public Undoable DuplicateCommand.createUndoActivity() {
		return new /*@AJHD PasteCommand*/PasteCommandUndo.UndoActivity(view());
	}

	//command undo contracts - consistent init of undo activities and affected figures
    pointcut commandExecuteInitUndo(DuplicateCommand acommand) :
		this(acommand)
		&& execution(void DuplicateCommand.execute())
		&& within(DuplicateCommand);

	
	//@see AlignCommandUndo
    before(DuplicateCommand acommand) : commandExecuteInitUndo(acommand) {
    	acommand.setUndoActivity(acommand.createUndoActivity());
    }
    
	//@see AlignCommandUndo
    before(DuplicateCommand acommand) : commandExecuteInitUndo(acommand) {
    	//@AJHD not really needed after untangling the functionality in the advised method
//    	FigureEnumeration figures = acommand.view().getFigureSelection().getData(StandardFigureSelection.TYPE);
//    	acommand.getUndoActivity().setAffectedFigures(figures);
    }
    
    
    //For a cleaner refactoring, we could use this advice only to store the return value of insertFigures,
    //and an after advice applied to execute() to set the affected figures for the undo support object.
    after(DuplicateCommand acommand) returning(FigureEnumeration fe): 
    	call(FigureEnumeration DuplicateCommand.insertFigures(FigureEnumeration, int, int)) &&
    	withincode(void DuplicateCommand.execute()) &&
    	this(acommand){
    		acommand.getUndoActivity().setAffectedFigures(fe);
    	}

}
