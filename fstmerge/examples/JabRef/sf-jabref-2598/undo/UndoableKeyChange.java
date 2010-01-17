
package net.sf.jabref.undo;

import javax.swing.undo.AbstractUndoableEdit;

import net.sf.jabref.BibtexDatabase;
import net.sf.jabref.Globals;


public class UndoableKeyChange extends AbstractUndoableEdit {

    private String entryId;
    private BibtexDatabase base;
    private String oldValue, newValue;

    public UndoableKeyChange(BibtexDatabase base, String entryId,
			     String oldValue, String newValue) {
	this.base = base;
	this.entryId = entryId;
	this.oldValue = oldValue;
	this.newValue = newValue;
    }

    public String getUndoPresentationName() {
	return Globals.lang("Undo")+": "+Globals.lang("change key");
    }

    public String getRedoPresentationName() {
	return Globals.lang("Redo")+": "+Globals.lang("change key");
    }

    public void undo() {
	super.undo();
	
	
	set(oldValue);
    }

    public void redo() {
	super.redo();

	
	set(newValue);
    }

    private void set(String to) {
	base.setCiteKeyForEntry(entryId, to);
    }

}
