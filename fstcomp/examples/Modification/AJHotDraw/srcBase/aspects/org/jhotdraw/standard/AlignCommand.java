/*
 * @(#)AlignCommand.java
 *
 * Project:		JHotdraw - a GUI framework for technical drawings
 *				http://www.jhotdraw.org
 *				http://jhotdraw.sourceforge.net
 * Copyright:	� by the original author(s) and all contributors
 * License:		Lesser GNU Public License (LGPL)
 *				http://www.opensource.org/licenses/lgpl-license.html
 */

package org.jhotdraw.standard;

import java.awt.Rectangle;

import org.jhotdraw.framework.DrawingEditor;
import org.jhotdraw.framework.Figure;
import org.jhotdraw.framework.FigureEnumeration;

/**
 * Align a selection of figures relative to each other.
 *
 * @version <$CURRENT_VERSION$>
 * 
 * @AJHD refactored: @author marin
 *  
 */
public class AlignCommand extends AbstractCommand {

	public static abstract class Alignment {
		/**
		 * align left sides
		 */
		public final static Alignment LEFTS = new Alignment("Lefts") {
			public void moveBy(Figure f, Rectangle anchor) {
				Rectangle rr = f.displayBox();
				f.moveBy(anchor.x-rr.x, 0);
			}
		};

		/**
		 * align centers (horizontally)
		 */
		public final static Alignment CENTERS = new Alignment("Centers") {
			public void moveBy(Figure f, Rectangle anchor) {
				Rectangle rr = f.displayBox();
				f.moveBy((anchor.x+anchor.width/2) - (rr.x+rr.width/2), 0);
			}
		};

		/**
		 * align right sides
		 */
		public final static Alignment RIGHTS = new Alignment("Rights") {
			public void moveBy(Figure f, Rectangle anchor) {
				Rectangle rr = f.displayBox();
				f.moveBy((anchor.x+anchor.width) - (rr.x+rr.width), 0);
			}
		};

		/**
		 * align tops
		 */
		public final static Alignment TOPS = new Alignment("Tops") {
			public void moveBy(Figure f, Rectangle anchor) {
				Rectangle rr = f.displayBox();
				f.moveBy(0, anchor.y-rr.y);
			}
		};

		/**
		 * align middles (vertically)
		 */
		public final static Alignment MIDDLES = new Alignment("Middles") {
			public void moveBy(Figure f, Rectangle anchor) {
				Rectangle rr = f.displayBox();
				f.moveBy(0, (anchor.y+anchor.height/2) - (rr.y+rr.height/2));
			}
		};

		/**
		 * align bottoms
		 */
		public final static Alignment BOTTOMS = new Alignment("Bottoms") {
			public void moveBy(Figure f, Rectangle anchor) {
				Rectangle rr = f.displayBox();
				f.moveBy(0, (anchor.y+anchor.height) - (rr.y+rr.height));
			}
		};

		private String myDescription;

		private Alignment(String newDescription) {
			setDescription(newDescription);
		}

		public String toString() {
			return getDescription();
		}

		public String getDescription() {
			return myDescription;
		}

		private void setDescription(String newDescription) {
			myDescription = newDescription;
		}

		public abstract void moveBy(Figure f, Rectangle anchor);
	}

	private Alignment myAlignment;

	/**
	 * Constructs an alignment command.
	 * @param newAlignment the alignment operation (LEFTS, CENTERS, RIGHTS, etc.)
	 * @param newDrawingEditor the DrawingEditor which manages the views
	 */
	public AlignCommand(Alignment newAlignment, DrawingEditor newDrawingEditor) {
		super(newAlignment.getDescription(), newDrawingEditor);
		setAlignment(newAlignment);
	}

	protected boolean isExecutableWithView() {
		return view().selectionCount() > 1;
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
//		// get selected figures in the order the figures have been selected
//		getUndoActivity().setAffectedFigures(view().selection());
		
//		@AJHD refactored - The original code (see below) used the associated UndoActivity for executing the core logic. 
//		((AlignCommand.UndoActivity)getUndoActivity()).alignAffectedFigures(getAlignment());
		
//		@AJHD added - The following code replaces the original one above that calls an action in 
//		UndoActivity to execute its core logic. This solution ensures obliviousness (The Command 
//		and Undo concerns are separated), but leads to code	duplication - this code is basically 
//		the alignAffectedFigures() method in UndoActivity.   
		FigureEnumeration fe = view().selection();
		Figure anchorFigure = fe.nextFigure();
		Rectangle r = anchorFigure.displayBox();

		while (fe.hasNextFigure()) {
			Figure f = fe.nextFigure();
			getAlignment().moveBy(f, r);
		}
		//@AJHD end added
		
//		@AJHD refactored
//		view().checkDamage();
	}

	protected void setAlignment(Alignment newAlignment) {
		myAlignment = newAlignment;
	}

	public Alignment getAlignment() {
		return myAlignment;
	}

//	@AJHD refactored
//	/**
//	 * Factory method for undo activity
//	 */
//	protected Undoable createUndoActivity() {
//		return new AlignCommand.UndoActivity(view(), getAlignment());
//	}

//	@AJHD refactored
//	public static class UndoActivity extends UndoableAdapter {
//		private Hashtable myOriginalPoints;
//		private Alignment myAppliedAlignment;
//
//		public UndoActivity(DrawingView newView, Alignment newAlignment) {
//			super(newView);
//			myOriginalPoints = new Hashtable();
//			setAppliedAlignment(newAlignment);
//			setUndoable(true);
//			setRedoable(true);
//		}
//
//		public boolean undo() {
//			if (!super.undo()) {
//				return false;
//			}
//
//			FigureEnumeration fe = getAffectedFigures();
//			while (fe.hasNextFigure()) {
//				Figure f = fe.nextFigure();
//				Point originalPoint = getOriginalPoint(f);
//				Point currentPoint = f.displayBox().getLocation();
//				// substract current lcoation to get to 0,0 and then move to original location
//				f.moveBy(-currentPoint.x + originalPoint.x,
//						 -currentPoint.y + originalPoint.y);
//			}
//
//			return true;
//		}
//
//		public boolean redo() {
//			if (!isRedoable()) {
//				return false;
//			}
//			alignAffectedFigures(getAppliedAlignment());
//			return true;
//		}
//
//		protected void setAppliedAlignment(Alignment newAlignment) {
//			myAppliedAlignment = newAlignment;
//		}
//
//		public Alignment getAppliedAlignment() {
//			return myAppliedAlignment;
//		}
//
//		protected void addOriginalPoint(Figure f) {
//			myOriginalPoints.put(f, f.displayBox().getLocation());
//		}
//
//		public Point getOriginalPoint(Figure f) {
//			return (Point)myOriginalPoints.get(f);
//		}
//
//		public void alignAffectedFigures(Alignment applyAlignment) {
//			FigureEnumeration fe = getAffectedFigures();
//			Figure anchorFigure = fe.nextFigure();
//			Rectangle r = anchorFigure.displayBox();
//
//			while (fe.hasNextFigure()) {
//				Figure f = fe.nextFigure();
//				applyAlignment.moveBy(f, r);
//			}
//		}
//
//		public void setAffectedFigures(FigureEnumeration fe) {
//			// first make copy of FigureEnumeration in superclass
//			super.setAffectedFigures(fe);
//			// then get new FigureEnumeration of copy to save aligment
//			FigureEnumeration copyFe = getAffectedFigures();
//			while (copyFe.hasNextFigure()) {
//				addOriginalPoint(copyFe.nextFigure());
//			}
//		}
//	}
}
