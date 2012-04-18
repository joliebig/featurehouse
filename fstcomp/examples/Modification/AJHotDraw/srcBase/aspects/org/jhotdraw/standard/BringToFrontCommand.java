/*
 * @(#)BringToFrontCommand.java
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
 * BringToFrontCommand brings the selected figures in the front of
 * the other figures.
 *
 * @see SendToBackCommand
 * @version <$CURRENT_VERSION$>
 * 
 * @AJHD refactored: @author marin
 * 
 */
public class BringToFrontCommand extends AbstractCommand {

	/**
	 * Constructs a bring to front command.
	 * @param name the command name
	 * @param newDrawingEditor the DrawingEditor which manages the views
	 */
	public BringToFrontCommand(String name, DrawingEditor newDrawingEditor) {
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
		
//		@AJHD refactored - The original code (see below) gets the affected figure from the 
//		UndoActivity just set. 
//		FigureEnumeration fe = getUndoActivity().getAffectedFigures();

//		@AJHD added - replaces the original code above that refers to UndoActivity 
		FigureEnumeration fe = view().selection();
//		@AJHD end added		
		
		while (fe.hasNextFigure()) {
			view().drawing().bringToFront(fe.nextFigure());
		}
//		@AJHD refactored
//		view().checkDamage();
	}

	public boolean isExecutableWithView() {
		return view().selectionCount() > 0;
	}

//	@AJHD refactored
//	protected Undoable createUndoActivity() {
//		return new BringToFrontCommand.UndoActivity(view());
//	}

//	@AJHD refactored
//	public static class UndoActivity extends SendToBackCommand.UndoActivity {
//		public UndoActivity(DrawingView newDrawingView) {
//			super(newDrawingView);
//		}
//
//		protected void sendToCommand(Figure f) {
//			getDrawingView().drawing().bringToFront(f);
//		}
//	}
}
