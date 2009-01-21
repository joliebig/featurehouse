package org.jhotdraw.ccconcerns.commands.undo;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.Hashtable;

import org.jhotdraw.framework.DrawingView;
import org.jhotdraw.framework.Figure;
import org.jhotdraw.framework.FigureEnumeration;
import org.jhotdraw.standard.AlignCommand;
import org.jhotdraw.standard.AlignCommand.Alignment;
import org.jhotdraw.util.Undoable;
import org.jhotdraw.util.UndoableAdapter;

/**
 * Undo support for AlignCommand. Some of the more general concerns
 * (ie, those that cover more Command elements) are in CommandUndo.
 * 
 * @author Marius Marin
 */
public aspect AlignCommandUndo {

	/**
	 * Factory method for undo activity
	 */
	/*@AJHD protected*/public Undoable AlignCommand.createUndoActivity() {
		//@AJHD refactored
//		return new AlignCommand.UndoActivity(view(), getAlignment());
		return new AlignCommandUndo.UndoActivity(view(), getAlignment());
	}
	
	
	// command undo contracts - consistent init of undo activities and affected figures
	// Captured join points: only the execution of AlignCommand's execute method
	// Exposed context: the AlignCommand of which the execute method is being run
    pointcut commandExecuteInitUndo(AlignCommand acommand) :
		this(acommand)
		&& execution(void AlignCommand.execute())
		&& within(AlignCommand);


    /*	Note:
	If the two pieces of advice are defined in the same aspect, 
	then there are two cases:
 	* If either are after advice, then the one that appears later 
 	  in the aspect has precedence over the one that appears earlier.
 	* Otherwise, then the one that appears earlier in the aspect has 
 	  precedence over the one that appears later. 
	 */

	/**
	 * CB: Init the undo activity for the executing command.
	 */
	before(AlignCommand acommand) : commandExecuteInitUndo(acommand) {
		acommand.setUndoActivity(acommand.createUndoActivity());
	}

	/**
	 * CB: Set the figures affected by the execution of the command to the associated undo 
	 * activity. It uses the same pointcut as the previus advice to init undo.
	 * Note: the order between this and the previous advice is important! The initialization of the
	 * undo activity has to happen before setting the affected figures for the UndoActivity object.  
	 */
	before(AlignCommand acommand) : commandExecuteInitUndo(acommand) {
		acommand.getUndoActivity().setAffectedFigures(acommand.view().selection());
	}

	
	public static class UndoActivity extends UndoableAdapter {
		private Hashtable myOriginalPoints;
		private Alignment myAppliedAlignment;

		public UndoActivity(DrawingView newView, Alignment newAlignment) {
			super(newView);
			myOriginalPoints = new Hashtable();
			setAppliedAlignment(newAlignment);
			setUndoable(true);
			setRedoable(true);
		}

		public boolean undo() {
			if (!super.undo()) {
				return false;
			}

			FigureEnumeration fe = getAffectedFigures();
			while (fe.hasNextFigure()) {
				Figure f = fe.nextFigure();
				Point originalPoint = getOriginalPoint(f);
				Point currentPoint = f.displayBox().getLocation();
				// substract current lcoation to get to 0,0 and then move to original location
				f.moveBy(-currentPoint.x + originalPoint.x,
						 -currentPoint.y + originalPoint.y);
			}

			return true;
		}

		public boolean redo() {
			if (!isRedoable()) {
				return false;
			}
			alignAffectedFigures(getAppliedAlignment());
			return true;
		}

		protected void setAppliedAlignment(Alignment newAlignment) {
			myAppliedAlignment = newAlignment;
		}

		public Alignment getAppliedAlignment() {
			return myAppliedAlignment;
		}

		protected void addOriginalPoint(Figure f) {
			myOriginalPoints.put(f, f.displayBox().getLocation());
		}

		public Point getOriginalPoint(Figure f) {
			return (Point)myOriginalPoints.get(f);
		}

		public void alignAffectedFigures(Alignment applyAlignment) {
			FigureEnumeration fe = getAffectedFigures();
			Figure anchorFigure = fe.nextFigure();
			Rectangle r = anchorFigure.displayBox();

			while (fe.hasNextFigure()) {
				Figure f = fe.nextFigure();
				applyAlignment.moveBy(f, r);
			}
		}

		public void setAffectedFigures(FigureEnumeration fe) {
			// first make copy of FigureEnumeration in superclass
			super.setAffectedFigures(fe);
			// then get new FigureEnumeration of copy to save aligment
			FigureEnumeration copyFe = getAffectedFigures();
			while (copyFe.hasNextFigure()) {
				addOriginalPoint(copyFe.nextFigure());
			}
		}
	}

}

