/*
 * @(#)PasteCommand.java
 *
 * Project:		JHotdraw - a GUI framework for technical drawings
 *				http://www.jhotdraw.org
 *				http://jhotdraw.sourceforge.net
 * Copyright:	� by the original author(s) and all contributors
 * License:		Lesser GNU Public License (LGPL)
 *				http://www.opensource.org/licenses/lgpl-license.html
 */

package org.jhotdraw.standard;

import org.jhotdraw.framework.*;
import org.jhotdraw.util.*;
import java.awt.*;

/**
 * Command to insert the clipboard into the drawing.
 *
 * @see Clipboard
 *
 * @version <$CURRENT_VERSION$>
 * 
 * @AJHD refactored: @author marin 
 */
public class PasteCommand extends FigureTransferCommand {

	/**
	 * Constructs a paste command.
	 * @param name the command name
	 * @param newDrawingEditor the DrawingEditor which manages the views
	 */
	public PasteCommand(String name, DrawingEditor newDrawingEditor) {
		super(name, newDrawingEditor);
	}

   /**
	 * @AJHD refactored: consistent condition check  
	 * @see CommandContracts
     */
	public void execute() {
//		@AJHD refactored
//		super.execute();
		Point lastClick = view().lastClick();
		FigureSelection selection = (FigureSelection)Clipboard.getClipboard().getContents();
		if (selection != null) {
//			@AJHD refactored
//			setUndoActivity(createUndoActivity());
//			@AJHD refactored
//			getUndoActivity().setAffectedFigures(
//				(FigureEnumerator)selection.getData(StandardFigureSelection.TYPE));

			//@AJHD added
			FigureEnumerator affectedFigures = (FigureEnumerator)selection.getData(StandardFigureSelection.TYPE);
			//@AJHD end added
			
			if (!/*@AJHD getUndoActivity().getAffectedFigures()*/affectedFigures.hasNextFigure()) {
//				@AJHD refactored
//				setUndoActivity(null);
				return;
			}

//			@AJHD - replaced (see below) to not refer the Undo support object 
//			Rectangle r = getBounds(getUndoActivity().getAffectedFigures());
			Rectangle r = getBounds(affectedFigures);

			//@AJHD added to be able to reuse the enumeration (cc)
			affectedFigures.reset();
			
			view().clearSelection();

			// get an enumeration of inserted figures
//			@AJHD - replaced (see below) to not refer the Undo support object
//			FigureEnumeration fe = insertFigures(getUndoActivity().getAffectedFigures(), lastClick.x-r.x, lastClick.y-r.y);
			FigureEnumeration fe = insertFigures(affectedFigures, lastClick.x-r.x, lastClick.y-r.y);
			
//			@AJHD refactored
//			getUndoActivity().setAffectedFigures(fe);
			
//			@AJHD refactored : the call is now executed independently of the condition: selection != null
//			This is for the sake of a more general refactoring solution (same pointcut+advice solution)
//			view().checkDamage();
		}
	}

	public boolean isExecutableWithView() {
		return Clipboard.getClipboard().getContents() != null;
	}

	private Rectangle getBounds(FigureEnumeration fe) {
		Rectangle r = fe.nextFigure().displayBox();
		while (fe.hasNextFigure()) {
			r.add(fe.nextFigure().displayBox());
		}
		return r;
	}

//	@AJHD refactored
//	/**
//	 * Factory method for undo activity
//	 */
//	protected Undoable createUndoActivity() {
//		return new PasteCommand.UndoActivity(view());
//	}

//	@AJHD refactored
//	public static class UndoActivity extends UndoableAdapter {
//
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
//			DeleteFromDrawingVisitor deleteVisitor = new DeleteFromDrawingVisitor(getDrawingView().drawing());
//			FigureEnumeration fe = getAffectedFigures();
//			while (fe.hasNextFigure()) {
//	    		fe.nextFigure().visit(deleteVisitor);
//			}
//
//			getDrawingView().clearSelection();
//
//			return true;
//		}
//
//		public boolean redo() {
//			// do not call execute directly as the selection might has changed
//			if (!isRedoable()) {
//				return false;
//			}
//
//			getDrawingView().clearSelection();
//			setAffectedFigures(getDrawingView().insertFigures(
//				getAffectedFigures(), 0, 0, false));
//
//			return true;
//		}
//	}
}
