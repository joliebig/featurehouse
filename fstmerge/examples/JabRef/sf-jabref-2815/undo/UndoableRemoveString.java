
package net.sf.jabref.undo;

import javax.swing.undo.AbstractUndoableEdit;

import net.sf.jabref.*;

public class UndoableRemoveString extends AbstractUndoableEdit {

    private BibtexDatabase base;
    private BibtexString string;
    private BasePanel panel;

    public UndoableRemoveString(BasePanel panel,
				BibtexDatabase base, BibtexString string) {
	this.base = base;
	this.string = string;
	this.panel = panel;
    }

    public String getUndoPresentationName() {
	return Globals.lang("Undo")+": "+Globals.lang("remove string ");
    }

    public String getRedoPresentationName() {
	return Globals.lang("Redo")+": "+Globals.lang("remove string ");
    }

    public void undo() {
	super.undo();
	
	
	try {
	    base.addString(string);
	} catch (KeyCollisionException ex) {
	    ex.printStackTrace();
	}

	panel.updateStringDialog();
    }

    public void redo() {
	super.redo();

	
	base.removeString(string.getId());

	panel.updateStringDialog();
    }



}
