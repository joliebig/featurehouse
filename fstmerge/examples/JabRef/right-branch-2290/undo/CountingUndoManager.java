
package net.sf.jabref.undo;

import javax.swing.undo.*;
import javax.swing.*;

import net.sf.jabref.BasePanel;

public class CountingUndoManager extends UndoManager {

    private int unchangedPoint = 0,
	current = 0;
    private BasePanel panel = null;

    public CountingUndoManager(BasePanel basePanel) {
	super();
	panel = basePanel;
    }

    public synchronized boolean addEdit(UndoableEdit edit) {
	current++;
	return super.addEdit(edit);
    }
    
    public synchronized void undo() throws CannotUndoException {
	    super.undo();
	    current--;
        panel.updateEntryEditorIfShowing();
        
        

    }

    public synchronized void redo() throws CannotUndoException {
	    super.redo();
	    current++;
        panel.updateEntryEditorIfShowing();
        
        
    }

    public synchronized void markUnchanged() {
    	unchangedPoint = current;
    }

    public boolean hasChanged() {
    	return !(current == unchangedPoint);
    }
}
