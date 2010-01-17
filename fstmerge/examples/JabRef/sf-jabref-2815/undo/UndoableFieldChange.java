
package net.sf.jabref.undo;

import javax.swing.undo.AbstractUndoableEdit;

import net.sf.jabref.BibtexEntry;
import net.sf.jabref.Util;


public class UndoableFieldChange extends AbstractUndoableEdit {

    private BibtexEntry entry;
    private String field;
    private String oldValue, newValue;

    public UndoableFieldChange(BibtexEntry entry, String field,
			       String oldValue, String newValue) {
	this.entry = entry;
	this.field = field;
	this.oldValue = oldValue;
	this.newValue = newValue;
    }

    public String getUndoPresentationName() {
	return "Undo: change field";
    }

    public String getRedoPresentationName() {
	return "Redo: change field";
    }

    public void undo() {
	super.undo();

	
	try {
          if (oldValue != null)
            entry.setField(field, oldValue);
          else
            entry.clearField(field);

	} catch (Throwable ex) {
	    Util.pr(ex.getMessage());
	}
    }

    public void redo() {
	super.redo();

	
	try {
          if (newValue != null)
            entry.setField(field, newValue);
          else
            entry.clearField(field);

	} catch (Throwable ex) {
	    Util.pr(ex.getMessage());
	}
    }



}
