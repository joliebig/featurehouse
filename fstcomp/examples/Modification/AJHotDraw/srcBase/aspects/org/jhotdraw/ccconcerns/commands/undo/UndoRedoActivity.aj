package org.jhotdraw.ccconcerns.commands.undo;

import org.jhotdraw.framework.DrawingView;
import org.jhotdraw.framework.FigureEnumeration;
//import org.jhotdraw.util.UndoRedoActivity;
import org.jhotdraw.util.Undoable;

/**
 * Refactoring of the org.jhotdraw.util.UndoRedoActivity that is an Undoable
 * wrapper (RL):
 * "it can be used to turn a UndoActivity into a RedoActivity.
 * In this case, the redo() method of an encapsulated activity is called when
 * the undo() is executed, and undo() when redo() is executed."
 * 
 * The original class does not have any clients, so the pointcuts will not match 
 * anything for now - it has to be updated with the calls that  
 * should pass through this redirection.
 * 
 * This solution might not be complete.
 * 
 * @author Marius Marin
 */
public aspect UndoRedoActivity {

//	private Undoable myReversedActivity;
//	
//	protected UndoRedoActivity(Undoable newReversedActivity) {
//		setReversedActivity(newReversedActivity);
//	}

//	public Undoable getReversedActivity() {
//		return myReversedActivity;
//	}
//	
//	public static Undoable createUndoRedoActivity(Undoable toBeReversed) {
//		// instead of reversing the reversed activity just return the original activity
//		if (toBeReversed instanceof UndoRedoActivity) {
//			return ((UndoRedoActivity)toBeReversed).getReversedActivity();
//		}
//		else {
//			return new UndoRedoActivity(toBeReversed);
//		}
//	}

	//Define a pointcut for each receiver-method in the receiver/target class
	pointcut callUndoableRedo(Undoable undoable) : 
		call(boolean Undoable.redo()) &&
		target(undoable) &&
		if(false); //just to have no match for now

//	public boolean undo() {
//		if (isRedoable()) {
//			return getReversedActivity().redo();
//		}
//		return false;
//	}

	boolean around(Undoable undoable) : callUndoableRedo(undoable) {
		if(undoable.isRedoable()) {
			return proceed(undoable);
		}
		return false;
	}

	
	pointcut callUndoableUndo(Undoable undoable) : 
		call(boolean Undoable.undo()) &&
		target(undoable) &&
		if(false); //just to have no match for now
	
//	public boolean redo() {
//		if (isUndoable()) {
//			return getReversedActivity().undo();
//		}
//		return false;
//	}

	boolean around(Undoable undoable) : callUndoableUndo(undoable) {
		if(undoable.isUndoable()) {
			return proceed(undoable);
		}
		return false;
	}

	
	pointcut callUndoableIsUndoable(Undoable undoable) : 
		call(boolean Undoable.isUndoable()) &&
		target(undoable) &&
		if(false); //just to have no match for now;
	
//	public boolean isRedoable() {
//		return getReversedActivity().isUndoable();
//	}
	
	boolean around(Undoable undoable) : callUndoableIsUndoable(undoable) {
		return proceed(undoable);
	}
	
	
	pointcut callUndoableIsRedoable(Undoable undoable) : 
		call(boolean Undoable.isRedoable()) &&
		target(undoable) &&
		if(false); //just to have no match for now

//	public boolean isUndoable() {
//		return getReversedActivity().isRedoable();
//	}

	boolean around(Undoable undoable) : callUndoableIsRedoable(undoable) {
		return proceed(undoable);
	}

	
	pointcut callUndoableSetUndoable(Undoable undoable, boolean newIsUndoable) : 
		call(void Undoable.setUndoable(boolean)) &&
		args(newIsUndoable) &&
		target(undoable) &&
		if(false); //just to have no match for now
	
//	public void setUndoable(boolean newIsUndoable) {
//		getReversedActivity().setRedoable(newIsUndoable);
//	}

	void around(Undoable undoable, boolean newIsUndoable) : callUndoableSetUndoable(undoable, newIsUndoable) {
		proceed(undoable, newIsUndoable);
	}

	
	pointcut callUndoableSetRedoable(Undoable undoable, boolean newIsRedoable) : 
		call(void Undoable.setRedoable(boolean)) &&
		args(newIsRedoable) &&
		target(undoable) &&
		if(false); //just to have no match for now
	
//	public void setRedoable(boolean newIsRedoable) {
//		getReversedActivity().setUndoable(newIsRedoable);
//	}
	boolean around(Undoable undoable, boolean newIsRedoable) : callUndoableSetRedoable(undoable, newIsRedoable) {
		return proceed(undoable, newIsRedoable);
	}

	
	pointcut callUndoableRelease(Undoable undoable) : 
		call(void Undoable.release()) &&
		target(undoable) &&
		if(false); //just to have no match for now
	
//	public void release() {
//		getReversedActivity().release();
//	}
	
	void around(Undoable undoable) : callUndoableRelease(undoable) {
		proceed(undoable);
	}
	
	
	pointcut callUndoableGetDrawingView(Undoable undoable) : 
		call(DrawingView Undoable.getDrawingView()) &&
		target(undoable) &&
		if(false); //just to have no match for now
	
//	public DrawingView getDrawingView() {
//		return getReversedActivity().getDrawingView();
//	}

	DrawingView around(Undoable undoable) : callUndoableGetDrawingView(undoable) {
		return proceed(undoable);
	}


	pointcut callUndoableSetAffectedFigures(Undoable undoable, FigureEnumeration newAffectedFigures) : 
		call(void Undoable.setAffectedFigures(FigureEnumeration)) &&
		args(newAffectedFigures) &&
		target(undoable) &&
		if(false); //just to have no match for now
	
//	public void setAffectedFigures(FigureEnumeration newAffectedFigures) {
//		getReversedActivity().setAffectedFigures(newAffectedFigures);
//	}
	
	void around(Undoable undoable, FigureEnumeration newAffectedFigures) : callUndoableSetAffectedFigures(undoable, newAffectedFigures) {
		proceed(undoable, newAffectedFigures);
	}
	

	pointcut callUndoableGetAffectedFigures(Undoable undoable) : 
		call(FigureEnumeration Undoable.getAffectedFigures()) &&
		target(undoable) &&
		if(false); //just to have no match for now
	
//	public FigureEnumeration getAffectedFigures() {
//		return getReversedActivity().getAffectedFigures();
//	}
	
	FigureEnumeration around(Undoable undoable) : callUndoableGetAffectedFigures(undoable) {
		return proceed(undoable);
	}


	pointcut callUndoableGetAffectedFiguresCount(Undoable undoable) : 
		call(FigureEnumeration Undoable.getAffectedFiguresCount()) &&
		target(undoable);

//	public int getAffectedFiguresCount() {
//		return getReversedActivity().getAffectedFiguresCount();
//	}

	int around(Undoable undoable) : callUndoableGetAffectedFiguresCount(undoable) {
		return proceed(undoable);
	}
}
