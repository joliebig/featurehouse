
package org.jhotdraw.util; 
import org.jhotdraw.framework.*; 
import org.jhotdraw.standard.FigureEnumerator; 
import org.jhotdraw.standard.ReverseFigureEnumerator; 
import org.jhotdraw.standard.StandardFigureSelection; 
import java.util.Collections; 
import java.util.List; 
public  class  UndoableAdapter  implements Undoable {
		private List myAffectedFigures;

		private boolean myIsUndoable;

		private boolean myIsRedoable;

		private DrawingView myDrawingView;

		public UndoableAdapter(DrawingView newDrawingView) {	setDrawingView(newDrawingView);	}

		public boolean undo() {	return isUndoable();	}

		public boolean redo() {	return isRedoable();	}

		public boolean isUndoable() {	return myIsUndoable;	}

		public void setUndoable(boolean newIsUndoable) {	myIsUndoable = newIsUndoable;	}

		public boolean isRedoable() {	return myIsRedoable;	}

		public void setRedoable(boolean newIsRedoable) {	myIsRedoable = newIsRedoable;	}

		public void setAffectedFigures(FigureEnumeration newAffectedFigures) {	if(newAffectedFigures == null) {	throw new IllegalArgumentException();	}	rememberFigures(newAffectedFigures);	}

		public FigureEnumeration getAffectedFigures() {	if(myAffectedFigures == null) {	return new FigureEnumerator(Collections.EMPTY_LIST);	}	else {	return new FigureEnumerator(CollectionsFactory.current().createList(myAffectedFigures));	}	}

		public FigureEnumeration getAffectedFiguresReversed() {	return new ReverseFigureEnumerator(CollectionsFactory.current().createList(myAffectedFigures));	}

		public int getAffectedFiguresCount() {	return myAffectedFigures.size();	}

		protected void rememberFigures(FigureEnumeration toBeRemembered) {	myAffectedFigures = CollectionsFactory.current().createList();	while (toBeRemembered.hasNextFigure()) {	myAffectedFigures.add(toBeRemembered.nextFigure());	}	}

		public void release() {	FigureEnumeration fe = getAffectedFigures();	while (fe.hasNextFigure()) {	fe.nextFigure().release();	}	setAffectedFigures(FigureEnumerator.getEmptyEnumeration());	}

		protected void duplicateAffectedFigures() {	setAffectedFigures(StandardFigureSelection.duplicateFigures(	getAffectedFigures(), getAffectedFiguresCount()));	}

		public DrawingView getDrawingView() {	return myDrawingView;	}

		protected void setDrawingView(DrawingView newDrawingView) {	myDrawingView = newDrawingView;	}


}
