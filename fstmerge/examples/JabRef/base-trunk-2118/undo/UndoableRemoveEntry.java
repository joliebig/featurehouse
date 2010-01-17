
package net.sf.jabref.undo;

import javax.swing.undo.*;
import net.sf.jabref.*;
import java.util.HashMap;


public class UndoableRemoveEntry extends AbstractUndoableEdit {

    private BibtexDatabase base;
    private BibtexEntry entry;
    private BasePanel panel;

    public UndoableRemoveEntry(BibtexDatabase base, BibtexEntry entry,
			       BasePanel panel) {
	this.base = base;
	this.entry = entry;
	this.panel = panel;
    }

    public String getUndoPresentationName() {
	return "Undo: remove entry";
    }

    public String getRedoPresentationName() {
	return "Redo: remove entry";
    }

    public void undo() {
	super.undo();

	
	try {
	    String id = Util.createNeutralId();
	    entry.setId(id);
	    base.insertEntry(entry);
	} catch (Throwable ex) {
          ex.printStackTrace();
	}
    }

    public void redo() {
	super.redo();

	
	try {
	    base.removeEntry(entry.getId());
	    
	    panel.ensureNotShowing(entry);
	} catch (Throwable ex) {
          ex.printStackTrace();
	}
    }



}
