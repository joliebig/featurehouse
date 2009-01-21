/*
 * @(#)DuplicateCommand.java
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
import org.jhotdraw.framework.FigureSelection;

/**
 * Duplicate the selection and select the duplicates.
 *
 * @version <$CURRENT_VERSION$>
 * 
 * @AJHD refactored: @author marin 
 */
public class DuplicateCommand extends FigureTransferCommand {

   /**
	* Constructs a duplicate command.
	* @param name the command name
	 * @param newDrawingEditor the DrawingEditor which manages the views
	*/
	public DuplicateCommand(String name, DrawingEditor newDrawingEditor) {
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
		FigureSelection selection = view().getFigureSelection();

		// create duplicate figure(s)
		FigureEnumeration figures = (FigureEnumeration)selection.getData(StandardFigureSelection.TYPE);
//		@AJHD refactored
//		getUndoActivity().setAffectedFigures(figures);

		view().clearSelection();
//		@AJHD - replaced, see below, to eliminate dependencies on undo support
//		getUndoActivity().setAffectedFigures(
//			insertFigures(getUndoActivity().getAffectedFigures(), 10, 10));
		insertFigures(figures, 10, 10);
		
//		@AJHD refactored
//		view().checkDamage();
	}

	protected boolean isExecutableWithView() {
		return view().selectionCount() > 0;
	}

//	@AJHD refactored
//	/**
//	 * Factory method for undo activity
//	 */
//	protected Undoable createUndoActivity() {
//		return new PasteCommand.UndoActivity(view());
//	}
}
