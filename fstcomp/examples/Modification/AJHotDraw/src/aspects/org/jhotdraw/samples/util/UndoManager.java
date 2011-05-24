
package org.jhotdraw.util; 
import org.jhotdraw.framework.DrawingView; 
import java.util.List; 
import java.util.Iterator; 
public  class  UndoManager {
		public static final int DEFAULT_BUFFER_SIZE = 20;

		private List redoStack;

		private List undoStack;

		private int maxStackCapacity;

		public UndoManager() {	this(DEFAULT_BUFFER_SIZE);	}

		public UndoManager(int newUndoStackSize) {	maxStackCapacity = newUndoStackSize;	undoStack = CollectionsFactory.current().createList(maxStackCapacity);	redoStack = CollectionsFactory.current().createList(maxStackCapacity);	}

		public void pushUndo(Undoable undoActivity) {	if (undoActivity.isUndoable()) {	removeFirstElementInFullList(undoStack);	undoStack.add(undoActivity);	}	else {	undoStack = CollectionsFactory.current().createList(maxStackCapacity);	}	}

		public void pushRedo(Undoable redoActivity) {	if (redoActivity.isRedoable()) {	removeFirstElementInFullList(redoStack);	if ((getRedoSize() == 0) || (peekRedo() != redoActivity)) {	redoStack.add(redoActivity);	}	}	else {	redoStack = CollectionsFactory.current().createList(maxStackCapacity);	}	}

		private void removeFirstElementInFullList(List l) {	if (l.size() >= maxStackCapacity) {	Undoable removedActivity = (Undoable)l.remove(0);	removedActivity.release();	}	}

		private Undoable getLastElement(List l) {	if (l.size() > 0) {	return (Undoable)l.get(l.size() - 1);	}	else {	return null;	}	}

		public boolean isUndoable() {	if (getUndoSize() > 0) {	return getLastElement(undoStack).isUndoable();	}	else {	return false;	}	}

		public boolean isRedoable() {	if (getRedoSize() > 0) {	return getLastElement(redoStack).isRedoable();	}	else {	return false;	}	}

		protected Undoable peekUndo() {	if (getUndoSize() > 0) {	return getLastElement(undoStack);	}	else {	return null;	}	}

		protected Undoable peekRedo() {	if (getRedoSize() > 0) {	return getLastElement(redoStack);	}	else {	return null;	}	}

		public int getUndoSize() {	return undoStack.size();	}

		public int getRedoSize() {	return redoStack.size();	}

		public Undoable popUndo() {	Undoable lastUndoable = peekUndo();	undoStack.remove(getUndoSize() - 1);	return lastUndoable;	}

		public Undoable popRedo() {	Undoable lastUndoable = peekRedo();	redoStack.remove(getRedoSize() - 1);	return lastUndoable;	}

		public void clearUndos() {	clearStack(undoStack);	}

		public void clearRedos() {	clearStack(redoStack);	}

		protected void clearStack(List clearStack) {	clearStack.clear();	}

		public void clearUndos(DrawingView checkDV) {	Iterator iter = undoStack.iterator();	while (iter.hasNext()) {	Undoable currentUndo = (Undoable)iter.next();	if (currentUndo.getDrawingView() == checkDV) {	iter.remove();	}	}	}

		public void clearRedos(DrawingView checkDV) {	Iterator iter = redoStack.iterator();	while (iter.hasNext()) {	Undoable currentRedo = (Undoable)iter.next();	if (currentRedo.getDrawingView() == checkDV) {	iter.remove();	}	}	}


}
