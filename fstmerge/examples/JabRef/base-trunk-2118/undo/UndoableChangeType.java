
package net.sf.jabref.undo;

import javax.swing.undo.*;
import java.util.HashMap;
import net.sf.jabref.*;


public class UndoableChangeType extends AbstractUndoableEdit {

    BibtexEntryType oldType, newType;
    BibtexEntry be;

    public UndoableChangeType(BibtexEntry be, BibtexEntryType oldType,
			      BibtexEntryType newType) {
	this.oldType = oldType;
	this.newType = newType;
	this.be = be;
    }

    public String getUndoPresentationName() {
	return "Undo: change type";
    }

    public String getRedoPresentationName() {
	return "Redo: change type";
    }

    public void undo() {
	super.undo();
	be.setType(oldType);
    }

    public void redo() {
	super.redo();
	be.setType(newType);
    }

}
