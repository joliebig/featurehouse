/*
 * @(#)FigureTransferCommand.java
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

import org.jhotdraw.util.*;
import org.jhotdraw.framework.*;

/**
 * Common base clase for commands that transfer figures
 * between a drawing and the clipboard.
 *
 * @version <$CURRENT_VERSION$>
 * 
 * @AJHD refactored: @author marin
 */
public abstract class FigureTransferCommand extends AbstractCommand {

	/**
	 * Constructs a drawing command.
	 * @param name the command name
	 * @param newDrawingEditor the DrawingEditor which manages the views
	 */
	protected FigureTransferCommand(String name, DrawingEditor newDrawingEditor) {
		super(name, newDrawingEditor);
	}

   /**
	* Deletes the selection from the drawing.
	*/
	protected void deleteFigures(FigureEnumeration fe) {
	   DeleteFromDrawingVisitor deleteVisitor = new DeleteFromDrawingVisitor(view().drawing());
		while (fe.hasNextFigure()) {
			fe.nextFigure().visit(deleteVisitor);
		}

		view().clearSelection();
	}

   /**
	* Copies the FigureEnumeration to the clipboard.
	*/
	protected void copyFigures(FigureEnumeration fe, int figureCount) {
		Clipboard.getClipboard().setContents(new StandardFigureSelection(fe, figureCount));
	}

   /**
	* Inserts an enumeration of figures and translates them by the
	* given offset.
	* @todo mrfloppy to investigate making this protected.  Looks like it would
	*       be no problem to me.  It was package scope.  I thought it safer to
	*       make it less restrictive just incase their was a reason for the
	*       package scope I didn't know about. dnoyeb.
	*       Bug - [ 673096 ] FigureTransferCommand has a wrong method
	*/
	public FigureEnumeration insertFigures(FigureEnumeration fe, int dx, int dy) {
		return view().insertFigures(fe, dx, dy, false);
	}
	
	
	/*
	 * 
	 * @AJHD Added method - See the (original) execute() methods (in 
	 * CutCommand, DeleteCommand ...) from which 
	 * this method was extracted.
	 */
	protected FigureEnumeration collectAffectedFigures() {
		FigureEnumeration fe = view().selection();
		List affected = CollectionsFactory.current().createList();
		Figure f;
		FigureEnumeration dfe;
		while (fe.hasNextFigure()) {
			f = fe.nextFigure();
			affected.add(0, f);
			dfe = f.getDependendFigures();
			if (dfe != null) {
				while (dfe.hasNextFigure()) {
					affected.add(0, dfe.nextFigure());
				}
			}
		}
		fe = new FigureEnumerator(affected);
		return fe;
	}
	
}


