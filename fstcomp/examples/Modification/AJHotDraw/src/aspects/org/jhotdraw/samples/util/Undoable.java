
package org.jhotdraw.util; 
import org.jhotdraw.framework.FigureEnumeration; 
import org.jhotdraw.framework.DrawingView; 
public  interface  Undoable {
		public boolean undo();

		public boolean redo();

		public boolean isUndoable();

		public void setUndoable(boolean newIsUndoable);

		public boolean isRedoable();

		public void setRedoable(boolean newIsRedoable);

		public void release();

		public DrawingView getDrawingView();

		public void setAffectedFigures(FigureEnumeration newAffectedFigures);

		public FigureEnumeration getAffectedFigures();

		public int getAffectedFiguresCount();


}
