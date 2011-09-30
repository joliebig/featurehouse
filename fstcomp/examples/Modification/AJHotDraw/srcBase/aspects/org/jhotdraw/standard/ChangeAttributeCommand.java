/*
 * @(#)ChangeAttributeCommand.java
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
import org.jhotdraw.framework.FigureAttributeConstant;
import org.jhotdraw.framework.FigureEnumeration;

/**
 * Command to change a named figure attribute.
 *
 * @version <$CURRENT_VERSION$>
 * 
 * @AJHD refactored: @author marin
 */
public  class ChangeAttributeCommand extends AbstractCommand {

	private FigureAttributeConstant fAttribute;
	private Object      fValue;

	/**
	 * Constructs a change attribute command.
	 * @param name the command name
	 * @param attribute the attribute to be changed
	 * @param value the new attribute value
	 * @param newDrawingEditor the DrawingEditor which manages the views
	 */
	public ChangeAttributeCommand(String name, FigureAttributeConstant attribute,
						   Object value, DrawingEditor newDrawingEditor) {
		super(name, newDrawingEditor);
		fAttribute = attribute;
		fValue = value;
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
			fe.nextFigure().setAttribute(fAttribute, fValue);
		}
//		@AJHD refactored
//		view().checkDamage();
	}

	public boolean isExecutableWithView() {
		return view().selectionCount() > 0;
	}

//	@AJHD refactored
//	/**
//	 * Factory method for undo activity
//	 */
//	protected Undoable createUndoActivity() {
//		return new ChangeAttributeCommand.UndoActivity(view(), fAttribute, fValue);
//	}

//	@AJHD refactored
//	public static class UndoActivity extends UndoableAdapter {
//		private FigureAttributeConstant myUndoAttribute;
//		private Hashtable	            myOriginalValues;
//		private Object                  myUndoValue;
//
//		public UndoActivity(DrawingView newDrawingView, FigureAttributeConstant newUndoAttribute, Object newUndoValue) {
//			super(newDrawingView);
//			myOriginalValues = new Hashtable();
//			setAttribute(newUndoAttribute);
//			setBackupValue(newUndoValue);
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
//				if (getOriginalValue(f) != null) {
//					f.setAttribute(getAttribute(), getOriginalValue(f));
//				}
//			}
//
//			return true;
//		}
//
//		public boolean redo() {
//			if (!isRedoable()) {
//				return false;
//			}
//
//			FigureEnumeration fe = getAffectedFigures();
//			while (fe.hasNextFigure()) {
//				Figure f = fe.nextFigure();
//				if (getBackupValue() != null) {
//					f.setAttribute(getAttribute(), getBackupValue());
//				}
//			}
//
//			return true;
//		}
//
//		protected void addOriginalValue(Figure affectedFigure, Object newOriginalValue) {
//			myOriginalValues.put(affectedFigure, newOriginalValue);
//		}
//
//		protected Object getOriginalValue(Figure lookupAffectedFigure) {
//			return myOriginalValues.get(lookupAffectedFigure);
//		}
//
//		protected void setAttribute(FigureAttributeConstant newUndoAttribute) {
//			myUndoAttribute = newUndoAttribute;
//		}
//
//		public FigureAttributeConstant getAttribute() {
//			return myUndoAttribute;
//		}
//
//		protected void setBackupValue(Object newUndoValue) {
//			myUndoValue = newUndoValue;
//		}
//
//		public Object getBackupValue() {
//			return myUndoValue;
//		}
//
//		public void release() {
//			super.release();
//			myOriginalValues = null;
//		}
//
//		public void setAffectedFigures(FigureEnumeration fe) {
//			// first make copy of FigureEnumeration in superclass
//			super.setAffectedFigures(fe);
//			// then get new FigureEnumeration of copy to save attributes
//			FigureEnumeration copyFe = getAffectedFigures();
//			while (copyFe.hasNextFigure()) {
//				Figure f = copyFe.nextFigure();
//				Object attributeValue = f.getAttribute(getAttribute());
//				if (attributeValue != null) {
//					addOriginalValue(f, attributeValue);
//				}
//			}
//		}
//	}
}
