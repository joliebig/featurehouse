/*
 * @(#)CutCommand.java
 *
 * Project:		JHotdraw - a GUI framework for technical drawings
 *				http://www.jhotdraw.org
 *				http://jhotdraw.sourceforge.net
 * Copyright:	� by the original author(s) and all contributors
 * License:		Lesser GNU Public License (LGPL)
 *				http://www.opensource.org/licenses/lgpl-license.html
 */

package org.jhotdraw.standard;

import java.util.List;

import org.jhotdraw.framework.DrawingEditor;
import org.jhotdraw.framework.FigureEnumeration;
import org.jhotdraw.util.CollectionsFactory;

/**
 * Delete the selection and move the selected figures to
 * the clipboard.
 *
 * @see org.jhotdraw.util.Clipboard
 *
 * @version <$CURRENT_VERSION$>
 * 
 * @AJHD refactored: @author marin
 */
public class CutCommand extends FigureTransferCommand {

	/**
	 * Constructs a cut command.
	 * @param name the command name
	 * @param newDrawingEditor the DrawingEditor which manages the views
	 */
	public CutCommand(String name, DrawingEditor newDrawingEditor) {
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

//		@AJHD refactored
//		UndoActivity ua = (UndoActivity) getUndoActivity();
//		ua.setSelectedFigures(view().selection());

//		@AJHD refactored - The original code (see below) gets the selected figures from the 
//		UndoActivity object. 
//		copyFigures(ua.getSelectedFigures(), ua.getSelectedFiguresCount());
		
//		@AJHD added - replaces the original code above that refers to UndoActivity
		FigureEnumeration selection = view().selection();
		//get the size of the enumeration ... pretty annoying
		
		List l = CollectionsFactory.current().createList();
		while(selection.hasNextFigure()) {
			l.add(selection.nextFigure());
		}
		selection.reset();
		
		copyFigures(selection, l.size());
//		@AJHD end added
		
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
	public boolean isExecutableWithView() {
		return view().selectionCount() > 0;
	}

//	@AJHD refactored
//	/**
//	 * Factory method for undo activity
//	 * @return Undoable
//	 */
//	protected Undoable createUndoActivity() {
//		return new CutCommand.UndoActivity(this);
//	}

//	@AJHD refactored
//	public static class UndoActivity extends UndoableAdapter {
//
//		private FigureTransferCommand myCommand;
//		private List mySelectedFigures;
//
//		/**
//		 * Constructor for <code>UndoActivity</code>.
//		 * @param newCommand
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
//				myCommand.insertFigures(getAffectedFiguresReversed(), 0, 0);
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
//				myCommand.copyFigures(getSelectedFigures(), getSelectedFiguresCount());
//				myCommand.deleteFigures(getAffectedFigures());
//				return true;
//			}
//
//			return false;
//		}
//
//		/**
//		 * Preserve the selection of figures the moment the command was executed.
//		 * @param newSelectedFigures
//		 */
//		public void setSelectedFigures(FigureEnumeration newSelectedFigures) {
//			// the enumeration is not reusable therefore a copy is made
//			// to be able to undo-redo the command several time
//			rememberSelectedFigures(newSelectedFigures);
//		}
//
//		/**
//		 * Preserve a copy of the enumeration in a private list.
//		 * @param toBeRemembered
//		 */
//		protected void rememberSelectedFigures(FigureEnumeration toBeRemembered) {
//			mySelectedFigures = CollectionsFactory.current().createList();
//			while (toBeRemembered.hasNextFigure()) {
//				mySelectedFigures.add(toBeRemembered.nextFigure());
//			}
//		}
//	
//		/**
//		 * Returns the selection of figures to perform the command on.
//		 * @return
//		 */
//		public FigureEnumeration getSelectedFigures() {
//			return new FigureEnumerator(
//				CollectionsFactory.current().createList(mySelectedFigures));
//		}
//
//		/**
//		 * Returns the size of the selection.
//		 * @return
//		 */
//		public int getSelectedFiguresCount() {
//			return mySelectedFigures.size();
//		}
//
//		/**
//		 * @see org.jhotdraw.util.UndoableAdapter#release()
//		 */
//		public void release() {
//			super.release();
//			FigureEnumeration fe = getSelectedFigures();
//			while (fe.hasNextFigure()) {
//				fe.nextFigure().release();
//			}
//			setSelectedFigures(FigureEnumerator.getEmptyEnumeration());
//		}
//	}
}
