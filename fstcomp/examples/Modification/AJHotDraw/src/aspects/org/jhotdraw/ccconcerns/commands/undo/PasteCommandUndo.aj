package org.jhotdraw.ccconcerns.commands.undo;

import org.jhotdraw.framework.DrawingView;
import org.jhotdraw.framework.FigureEnumeration;
import org.jhotdraw.framework.FigureSelection;
import org.jhotdraw.standard.DeleteFromDrawingVisitor;
import org.jhotdraw.standard.FigureEnumerator;
import org.jhotdraw.standard.PasteCommand;
import org.jhotdraw.standard.StandardFigureSelection;
import org.jhotdraw.util.Clipboard;
import org.jhotdraw.util.Command;
import org.jhotdraw.util.Undoable;
import org.jhotdraw.util.UndoableAdapter;


/**
 * Undo support for PasteCommand. Some of the more general concerns
 * (ie, those that cover more Command elements) are in CommandUndo.
 * 
 * @author Marius Marin
 */
public privileged aspect PasteCommandUndo {

	/**
	 * Factory method for undo activity
	 */
	/*@AJHD protected*/public Undoable PasteCommand.createUndoActivity() {
		return new /*@AJHD PasteCommand*/PasteCommandUndo.UndoActivity(view());
	}
	
	
	//I would have preferred to advise the execute method and not the 
	//call to the cliboard's method - that would have been a better 
	//solution reflecting the intent of the concern. It would also have
	//been better for pointcut/advice generalization purposes - see the other Command-s.
	after(PasteCommand acommand) returning(Object selection) : 
		call(Object Clipboard.getContents()) &&
		withincode(void PasteCommand.execute()) &&
		this(acommand) {
			figSelection = (FigureSelection)selection;
			if(selection != null) {
				acommand.setUndoActivity(acommand.createUndoActivity());
				FigureEnumerator fe = (FigureEnumerator)((FigureSelection)selection).getData(StandardFigureSelection.TYPE); 
				acommand.getUndoActivity().setAffectedFigures(fe);
				
				fe.reset(); //cc - added in order to be able to reuse the enumeration in the condition check 
				
				if(!fe.hasNextFigure())
					acommand.setUndoActivity(null);
					
			}
	}

	//save the figures affected the insertion operation 
	private FigureEnumeration affectedFigures;

	private FigureSelection figSelection;
	
	after(Command acommand) returning(FigureEnumeration affected) : 
		call(FigureEnumeration PasteCommand.insertFigures(FigureEnumeration, int, int)) &&
		withincode(void PasteCommand.execute()) &&
		this(acommand) {
			affectedFigures = affected;
	}
    
	pointcut commandExecute(PasteCommand acommand) :
		this(acommand)
		&& execution(void PasteCommand.execute())
		&& within(PasteCommand);

	after(PasteCommand acommand) : commandExecute(acommand) {
		//added an extra check
		if(figSelection!=null && affectedFigures != null) {
			acommand.getUndoActivity().setAffectedFigures(affectedFigures);
		}
	}
	
	
	public static class UndoActivity extends UndoableAdapter {

		public UndoActivity(DrawingView newDrawingView) {
			super(newDrawingView);
			setUndoable(true);
			setRedoable(true);
		}

		public boolean undo() {
			if (!super.undo()) {
				return false;
			}

			DeleteFromDrawingVisitor deleteVisitor = new DeleteFromDrawingVisitor(getDrawingView().drawing());
			FigureEnumeration fe = getAffectedFigures();
			while (fe.hasNextFigure()) {
	    		fe.nextFigure().visit(deleteVisitor);
			}

			getDrawingView().clearSelection();

			return true;
		}

		public boolean redo() {
			// do not call execute directly as the selection might has changed
			if (!isRedoable()) {
				return false;
			}

			getDrawingView().clearSelection();
			setAffectedFigures(getDrawingView().insertFigures(
				getAffectedFigures(), 0, 0, false));

			return true;
		}
	}


}

