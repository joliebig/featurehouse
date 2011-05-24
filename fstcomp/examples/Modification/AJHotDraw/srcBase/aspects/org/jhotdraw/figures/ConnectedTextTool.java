/*
 * @(#)ConnectedTextTool.java
 *
 * Project:		JHotdraw - a GUI framework for technical drawings
 *				http://www.jhotdraw.org
 *				http://jhotdraw.sourceforge.net
 * Copyright:	� by the original author(s) and all contributors
 * License:		Lesser GNU Public License (LGPL)
 *				http://www.opensource.org/licenses/lgpl-license.html
 */

package org.jhotdraw.figures;

import java.awt.event.MouseEvent;

import org.jhotdraw.framework.DrawingEditor;
import org.jhotdraw.framework.DrawingView;
import org.jhotdraw.framework.Figure;
import org.jhotdraw.framework.FigureEnumeration;
import org.jhotdraw.standard.FigureTransferCommand;
import org.jhotdraw.standard.TextHolder;

/**
 * Tool to create new or edit existing text figures.
 * A new text figure is connected with the clicked figure.
 *
 * @see org.jhotdraw.standard.TextHolder
 *
 * @version <$CURRENT_VERSION$>
 * 
 * @AJHD refactored: @author marin
 */
public  class ConnectedTextTool extends TextTool {

	private Figure myConnectedFigure;

	public ConnectedTextTool(DrawingEditor editor, Figure prototype) {
		super(editor, prototype);
	}

	/**
	 * If the pressed figure is a TextHolder it can be edited otherwise
	 * a new text figure is created.
	 */
	public void mouseDown(MouseEvent e, int x, int y) {
		super.mouseDown(e, x, y);

		if (getTypingTarget() != null) {
			TextHolder textHolder = getTypingTarget();
			setConnectedFigure(drawing().findFigureInsideWithout(x, y, textHolder.getRepresentingFigure()));
			if ((getConnectedFigure() != null) && (textHolder != null) && (getConnectedFigure().getTextHolder() != textHolder)) {
				textHolder.connect(getConnectedFigure().getDecoratedFigure());
				getConnectedFigure().addDependendFigure(getAddedFigure());
			}
		}
	}

	protected void endEdit() {
		super.endEdit();
		if ((getUndoActivity() != null) && (getUndoActivity() instanceof ConnectedTextTool.UndoActivity)) {
			((ConnectedTextTool.UndoActivity)getUndoActivity()).setConnectedFigure(getConnectedFigure());
		}
		else if ((getConnectedFigure() != null) && isDeleteTextFigure()) {
			getConnectedFigure().removeDependendFigure(getAddedFigure());
		}
	}

	protected void setConnectedFigure(Figure pressedFigure) {
		myConnectedFigure = pressedFigure;
	}

	public Figure getConnectedFigure() {
		return myConnectedFigure;
	}

	/**
	 * If the pressed figure is a TextHolder it can be edited otherwise
	 * a new text figure is created.
	 */
	public void activate() {
		super.activate();
		setConnectedFigure(null);
	}

//	@AJHD refactored
//	protected Undoable createDeleteUndoActivity() {
//		FigureTransferCommand cmd = new DeleteCommand("Delete", editor());
//		return new DeleteUndoActivity(cmd, getConnectedFigure());
//	}

//	@AJHD refactored
//	/**
//	 * Factory method for undo activity
//	 */
//	protected Undoable createUndoActivity() {
//		return new ConnectedTextTool.UndoActivity(view(), getTypingTarget().getText());
//	}

	public static class UndoActivity extends TextTool.UndoActivity {
		private Figure myConnectedFigure;

		public UndoActivity(DrawingView newDrawingView, String newOriginalText) {
			super(newDrawingView, newOriginalText);
		}

		/*
		 * Undo the activity
		 * @return true if the activity could be undone, false otherwise
		 */
		public boolean undo() {
			if (!super.undo()) {
				return false;
			}

			FigureEnumeration fe = getAffectedFigures();
			while (fe.hasNextFigure()) {
				Figure currentFigure = fe.nextFigure();

				if (currentFigure.getTextHolder() != null) {
					// the text figure didn't exist before
					if (!isValidText(getOriginalText())) {
						currentFigure.getTextHolder().disconnect(getConnectedFigure());
					}
					// the text figure did exist but was remove
					else if (!isValidText(getBackupText())) {
						currentFigure.getTextHolder().connect(getConnectedFigure());
					}
				}
			}

			return true;
		}

		/*
		 * Redo the activity
		 * @return true if the activity could be redone, false otherwise
		 */
		public boolean redo() {
			if (!super.redo()) {
				return false;
			}

			FigureEnumeration fe = getAffectedFigures();
			while (fe.hasNextFigure()) {
				Figure currentFigure = fe.nextFigure();
				if (currentFigure.getTextHolder() != null) {
					// the text figure did exist but was remove
					if (!isValidText(getBackupText())) {
						currentFigure.getTextHolder().disconnect(getConnectedFigure());
					}
					// the text figure didn't exist before
					else if (!isValidText(getOriginalText())) {
						currentFigure.getTextHolder().connect(getConnectedFigure());
					}
				}
			}

			return true;
		}

		public void setConnectedFigure(Figure newConnectedFigure) {
			myConnectedFigure = newConnectedFigure;
		}

		public Figure getConnectedFigure() {
			return myConnectedFigure;
		}
	}

	/**
	 * This class
	 */
	//@AJHD refactored - the DeleteCommand.UndoActivity has moved to an aspect from where 
	//it is declared as super class for this undo support.
	public static class DeleteUndoActivity /*@AJHD extends DeleteCommand.UndoActivity*/ {
		private Figure myConnectedFigure;

		public DeleteUndoActivity(FigureTransferCommand cmd, Figure newConnectedFigure) {
			super(cmd);
			setConnectedFigure(newConnectedFigure);
		}

		public boolean undo() {
			if (!super.undo()) {
				return false;
			}

			FigureEnumeration fe = getAffectedFigures();
			while (fe.hasNextFigure()) {
				Figure currentFigure = fe.nextFigure();
				if (currentFigure.getTextHolder() != null) {
					currentFigure.getTextHolder().connect(getConnectedFigure().getDecoratedFigure());
				}
			}

			return true;
		}

		public boolean redo() {
			if (!super.redo()) {
				return false;
			}

			FigureEnumeration fe = getAffectedFigures();
			while (fe.hasNextFigure()) {
				Figure currentFigure = fe.nextFigure();
				if (currentFigure.getTextHolder() != null) {
					currentFigure.getTextHolder().disconnect(getConnectedFigure().getDecoratedFigure());
				}
			}

			return true;
		}

		public void setConnectedFigure(Figure newConnectedFigure) {
			myConnectedFigure = newConnectedFigure;
		}

		public Figure getConnectedFigure() {
			return myConnectedFigure;
		}
	}
}
