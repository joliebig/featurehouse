
package net.sf.jabref.undo; 

import javax.swing.undo.AbstractUndoableEdit; 

import net.sf.jabref.BasePanel; 
import net.sf.jabref.BibtexDatabase; 
import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.Util; 


public  class  UndoableInsertEntry  extends AbstractUndoableEdit {
	

    private BibtexDatabase base;

	
    private BibtexEntry entry;

	
    private BasePanel panel;

	

    public UndoableInsertEntry(BibtexDatabase base, BibtexEntry entry,
			       BasePanel panel) {
	this.base = base;
	this.entry = entry;
	this.panel = panel;
    }


	

    public String getUndoPresentationName() {
	return "Undo: insert entry";
    }


	

    public String getRedoPresentationName() {
	return "Redo: insert entry";
    }


	

    public void undo() {
	super.undo();

	
	try {
	    base.removeEntry(entry.getId());
	    
	    panel.ensureNotShowing(entry);
	} catch (Throwable ex) {
          ex.printStackTrace();
	}
    }


	

    public void redo() {
	super.redo();

	
	try {
          String id = Util.createNeutralId();
	    entry.setId(id);
	    base.insertEntry(entry);
	} catch (Throwable ex) {
          ex.printStackTrace();
	}
    }



}
