package org.jhotdraw.ccconcerns.commands.undo;

import java.util.List;

import org.jhotdraw.figures.GroupCommand;
import org.jhotdraw.figures.GroupFigure;
import org.jhotdraw.framework.DrawingView;
import org.jhotdraw.framework.Figure;
import org.jhotdraw.framework.FigureEnumeration;
import org.jhotdraw.standard.FigureEnumerator;
import org.jhotdraw.util.CollectionsFactory;
import org.jhotdraw.util.Undoable;
import org.jhotdraw.util.UndoableAdapter;

/**
 * Undo support for GroupCommand. Some of the more general concerns
 * (ie, those that cover more Command elements) are in CommandUndo.
 * 
 * @author Marius Marin
 */
public aspect GroupCommandUndo {

	/**
	 * Factory method for undo activity
	 */
	/*@AJHD protected*/public Undoable GroupCommand.createUndoActivity() {
		return new /*@AJHD GroupCommand*/GroupCommandUndo.UndoActivity(view());
	}

	
	//command undo contracts - consistent init of undo activities and affected figures
    pointcut commandExecuteInitUndo(GroupCommand acommand) :
		this(acommand)
		&& execution(void GroupCommand.execute())
		&& within(GroupCommand);

	//@see AlignCommandUndo
    before(GroupCommand acommand) : commandExecuteInitUndo(acommand) {
    	acommand.setUndoActivity(acommand.createUndoActivity());
    }
    
    //Use this variable to store the figure obtained after the grouping 
    //operation - this will be the new "affected figure" used by undo
    //This solution is quite a hack ...
    private Figure gFigure;
    after(Figure figure) : call(void DrawingView.addToSelection(Figure)) &&
    	withincode(void GroupCommand.groupFigures()) &&
    	args(figure) {
    	gFigure = figure;
    }
    
	//@see AlignCommandUndo
    before(GroupCommand acommand) : commandExecuteInitUndo(acommand) {
//    	@AJHD not needed after the refactoring of the groupFigures() method
//		acommand.getUndoActivity().setAffectedFigures(acommand.view().selection());
    	
	}

    after(GroupCommand acommand) : commandExecuteInitUndo(acommand) {
		List affectedFigures = CollectionsFactory.current().createList();
		affectedFigures.add(gFigure);
		acommand.getUndoActivity().setAffectedFigures(new FigureEnumerator(affectedFigures));
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

			getDrawingView().clearSelection();

			// orphan group figure(s)
			getDrawingView().drawing().orphanAll(getAffectedFigures());

			// create a new collection with the grouped figures as elements
			List affectedFigures = CollectionsFactory.current().createList();

			FigureEnumeration fe = getAffectedFigures();
			while (fe.hasNextFigure()) {
				Figure currentFigure = fe.nextFigure();
				// add contained figures
				getDrawingView().drawing().addAll(currentFigure.figures());
				getDrawingView().addToSelectionAll(currentFigure.figures());

				FigureEnumeration groupedFigures = currentFigure.figures();
				while (groupedFigures.hasNextFigure()) {
					affectedFigures.add(groupedFigures.nextFigure());
				}
			}

			setAffectedFigures(new FigureEnumerator(affectedFigures));

			return true;
		}

		public boolean redo() {
			// do not call execute directly as the selection might has changed
			if (isRedoable()) {
				groupFigures();
				return true;
			}

			return false;
		}

		public void groupFigures() {
			getDrawingView().drawing().orphanAll(getAffectedFigures());
			getDrawingView().clearSelection();

			// add new group figure instead
			GroupFigure group = new GroupFigure();
			group.addAll(getAffectedFigures());

			Figure figure = getDrawingView().drawing().add(group);
			getDrawingView().addToSelection(figure);

			// create a new collection with the new group figure as element
			List affectedFigures = CollectionsFactory.current().createList();
			affectedFigures.add(figure);
			setAffectedFigures(new FigureEnumerator(affectedFigures));
		}
	}
	
}

