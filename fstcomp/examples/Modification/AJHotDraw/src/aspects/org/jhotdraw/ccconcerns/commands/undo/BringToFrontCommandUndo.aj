package org.jhotdraw.ccconcerns.commands.undo;

import org.jhotdraw.framework.DrawingView;
import org.jhotdraw.framework.Figure;
import org.jhotdraw.standard.BringToFrontCommand;
import org.jhotdraw.standard.SendToBackCommand;
import org.jhotdraw.util.Undoable;

/**
 * 
 * Undo support for BringToFrontCommand. Some of the more general concerns
 * (ie, those that cover more Command elements) are in CommandUndo.
 * 
 * @author Marius Marin
 */
public aspect BringToFrontCommandUndo {

	/*@AJHD protected*/public Undoable BringToFrontCommand.createUndoActivity() {
		//@AJHD refactored
//		return new BringToFrontCommand.UndoActivity(view());
		return new BringToFrontCommandUndo.UndoActivity(view());
	}
	
	//@see AlignCommandUndo 
    pointcut commandExecuteInitUndo(BringToFrontCommand acommand) :
		this(acommand)
		&& execution(void BringToFrontCommand.execute())
		&& within(BringToFrontCommand);


	//@see AlignCommandUndo
    before(BringToFrontCommand acommand) : commandExecuteInitUndo(acommand) {
		acommand.setUndoActivity(acommand.createUndoActivity());
	}

	//@see AlignCommandUndo
    before(BringToFrontCommand acommand) : commandExecuteInitUndo(acommand) {
		acommand.getUndoActivity().setAffectedFigures(acommand.view().selection());
	}

	
	public static class UndoActivity extends SendToBackCommand.UndoActivity {
		public UndoActivity(DrawingView newDrawingView) {
			super(newDrawingView);
		}

		protected void sendToCommand(Figure f) {
			getDrawingView().drawing().bringToFront(f);
		}
	}

}

