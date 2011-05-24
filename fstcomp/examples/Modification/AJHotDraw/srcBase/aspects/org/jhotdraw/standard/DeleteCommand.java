/*
 * @(#)DeleteCommand.java
 *
 * Project:		JHotdraw - a GUI framework for technical drawings
 *				http://www.jhotdraw.org
 *				http://jhotdraw.sourceforge.net
 * Copyright:	� by the original author(s) and all contributors
 * License:		Lesser GNU Public License (LGPL)
 *				http://www.opensource.org/licenses/lgpl-license.html
 */

package org.jhotdraw.standard;

import org.jhotdraw.framework.DrawingEditor;
import org.jhotdraw.framework.FigureEnumeration;

/**
 * Command to delete the selection.
 *
 * @version <$CURRENT_VERSION$>
 * 
 * @AJHD refactored: @author marin 
 */
public class DeleteCommand extends FigureTransferCommand {

	/**
	 * Constructs a delete command.
	 * @param name the command name
	 * @param newDrawingEditor the DrawingEditor which manages the views
	 */
	public DeleteCommand(String name, DrawingEditor newDrawingEditor) {
		super(name, newDrawingEditor);
	}

	/**
	 * @see org.jhotdraw.util.Command#execute()
	 *
	 * @AJHD refactored: consistent condition check  
	 * @see CommandContracts
     */
	public void execute() {
//		@AJHD refactored
//		super.execute();
//		@AJHD refactored
//		setUndoActivity(createUndoActivity());
		
		//@AJHD I extracted part of the bugfix below to a separate method, 
		//and invoked that method instead. The
		//bug fix is a clone in several Commands (CutCommant, DeleteCommand), so I 
		//declared the extracted method in the super class.
		/* ricardo_padilha: bugfix for correct delete/undelete behavior
		 * When enumerating the affected figures we must not forget the dependent
		 * figures, since they are deleted as well! 
		 */
//		FigureEnumeration fe = view().selection();
//		List affected = CollectionsFactory.current().createList();
//		Figure f;
//		FigureEnumeration dfe;
//		while (fe.hasNextFigure()) {
//			f = fe.nextFigure();
//			affected.add(0, f);
//			dfe = f.getDependendFigures();
//			if (dfe != null) {
//				while (dfe.hasNextFigure()) {
//					affected.add(0, dfe.nextFigure());
//				}
//			}
//		}
//		fe = new FigureEnumerator(affected);
		
		//@AJHD added - replaced the code above with this call - method extraction; 
		FigureEnumeration fe = collectAffectedFigures();
		//@AJHD end added

//		@AJHD refactored
//		getUndoActivity().setAffectedFigures(fe);
		/* ricardo_padilha: end of bugfix */
		
//		@AJHD refactored - The original code (see below) gets the affected figures from the 
//		UndoActivity object. 
//		deleteFigures(getUndoActivity().getAffectedFigures());
//		@AJHD added - replaces the original code above that refers to UndoActivity
		deleteFigures(fe);
//		@AJHD end added
		
//		@AJHD refactored
//		view().checkDamage();
	}
	
	

	/**
	 * @see org.jhotdraw.standard.AbstractCommand#isExecutableWithView()
	 */
	protected boolean isExecutableWithView() {
		return view().selectionCount() > 0;
	}

//	@AJHD refactored
//	/**
//	 * Factory method for undo activity
//	 * @return Undoable
//	 */
//	protected Undoable createUndoActivity() {
//		return new DeleteCommand.UndoActivity(this);
//	}

//	@AJHD refactored
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
