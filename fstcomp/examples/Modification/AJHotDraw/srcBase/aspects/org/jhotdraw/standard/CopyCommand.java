/*
 * @(#)CopyCommand.java
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

/**
 * Copy the selection to the clipboard.
 *
 * @see Clipboard
 *
 * @version <$CURRENT_VERSION$>
 * 
 * @AJHD refactored: @author marin
 */
public class CopyCommand extends FigureTransferCommand {

	/**
	 * Constructs a copy command.
	 * @param name the command name
	 * @param newDrawingEditor the DrawingEditor which manages the views
	 */
	public CopyCommand(String name, DrawingEditor newDrawingEditor) {
		super(name, newDrawingEditor);
	}

   /**
	 * @AJHD refactored: consistent condition check  
	 * @see CommandContracts
     */
	public void execute() {
//		@AJHD refactored
//		super.execute();
		copyFigures(view().selection(), view().selectionCount());
	}

	protected boolean isExecutableWithView() {
		return view().selectionCount() > 0;
	}
}
