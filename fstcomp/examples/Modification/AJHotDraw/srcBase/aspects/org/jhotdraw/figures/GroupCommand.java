/*
 * @(#)GroupCommand.java
 *
 * Project:		JHotdraw - a GUI framework for technical drawings
 *				http://www.jhotdraw.org
 *				http://jhotdraw.sourceforge.net
 * Copyright:	� by the original author(s) and all contributors
 * License:		Lesser GNU Public License (LGPL)
 *				http://www.opensource.org/licenses/lgpl-license.html
 */

package org.jhotdraw.figures;

import org.jhotdraw.framework.DrawingEditor;
import org.jhotdraw.framework.Figure;
import org.jhotdraw.framework.FigureEnumeration;
import org.jhotdraw.standard.AbstractCommand;

/**
 * Command to group the selection into a GroupFigure.
 *
 * @see GroupFigure
 *
 * @version <$CURRENT_VERSION$>
 * 
 * @AJHD refactored: @author marin 
 */
public  class GroupCommand extends AbstractCommand {

   /**
	 * Constructs a group command.
	 * @param name the command name
	 * @param newDrawingEditor the DrawingEditor which manages the views
	 */
	public GroupCommand(String name, DrawingEditor newDrawingEditor) {
		super(name, newDrawingEditor);
	}

   /**
	 * @AJHD refactored: consistent condition check  
	 * @see CommandContracts
     */
	public void execute() {
//		@AJHD refactored
//		super.execute();
//		@AJHD refactored
//		setUndoActivity(createUndoActivity());
//		@AJHD refactored
//		getUndoActivity().setAffectedFigures(view().selection());
		
		//@AJHD replaced (see below) for not referring the Undo support
		//for the core logic
//		((GroupCommand.UndoActivity)getUndoActivity()).groupFigures();
		groupFigures();
		
//		@AJHD refactored
//		view().checkDamage();
	}

	//@AJHD added for a clear separation of concerns - Most of the method's
	//logic comes from the undo support class (UndoActivity), which moved to the aspect
	//responsible for undo.
	public void groupFigures() {
		FigureEnumeration selection = view().selection(); 
		view().drawing().orphanAll(selection);
		view().clearSelection();

		//@AJHD ... to reuse it ...
		selection.reset();
		
		// add new group figure instead
		GroupFigure group = new GroupFigure();
		group.addAll(selection);

		Figure figure = view().drawing().add(group);
		view().addToSelection(figure);

//		@AJHD refactored
		// create a new collection with the new group figure as element
//		List affectedFigures = CollectionsFactory.current().createList();
//		affectedFigures.add(figure);
//		setAffectedFigures(new FigureEnumerator(affectedFigures));
	}
	//@AJHD end added	
	
	
	public boolean isExecutableWithView() {
		return view().selectionCount() > 1;
	}

//	@AJHD refactored
//	/**
//	 * Factory method for undo activity
//	 */
//	protected Undoable createUndoActivity() {
//		return new GroupCommand.UndoActivity(view());
//	}

//	@AJHD refactored
//	public static class UndoActivity extends UndoableAdapter {
//		public UndoActivity(DrawingView newDrawingView) {
//			super(newDrawingView);
//			setUndoable(true);
//			setRedoable(true);
//		}
//
//		public boolean undo() {
//			if (!super.undo()) {
//				return false;
//			}
//
//			getDrawingView().clearSelection();
//
//			// orphan group figure(s)
//			getDrawingView().drawing().orphanAll(getAffectedFigures());
//
//			// create a new collection with the grouped figures as elements
//			List affectedFigures = CollectionsFactory.current().createList();
//
//			FigureEnumeration fe = getAffectedFigures();
//			while (fe.hasNextFigure()) {
//				Figure currentFigure = fe.nextFigure();
//				// add contained figures
//				getDrawingView().drawing().addAll(currentFigure.figures());
//				getDrawingView().addToSelectionAll(currentFigure.figures());
//
//				FigureEnumeration groupedFigures = currentFigure.figures();
//				while (groupedFigures.hasNextFigure()) {
//					affectedFigures.add(groupedFigures.nextFigure());
//				}
//			}
//
//			setAffectedFigures(new FigureEnumerator(affectedFigures));
//
//			return true;
//		}
//
//		public boolean redo() {
//			// do not call execute directly as the selection might has changed
//			if (isRedoable()) {
//				groupFigures();
//				return true;
//			}
//
//			return false;
//		}
//
//		public void groupFigures() {
//			getDrawingView().drawing().orphanAll(getAffectedFigures());
//			getDrawingView().clearSelection();
//
//			// add new group figure instead
//			GroupFigure group = new GroupFigure();
//			group.addAll(getAffectedFigures());
//
//			Figure figure = getDrawingView().drawing().add(group);
//			getDrawingView().addToSelection(figure);
//
//			// create a new collection with the new group figure as element
//			List affectedFigures = CollectionsFactory.current().createList();
//			affectedFigures.add(figure);
//			setAffectedFigures(new FigureEnumerator(affectedFigures));
//		}
//	}
}
