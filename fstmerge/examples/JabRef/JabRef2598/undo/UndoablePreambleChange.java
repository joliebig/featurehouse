
package net.sf.jabref.undo; 

import javax.swing.undo.AbstractUndoableEdit; 

import net.sf.jabref.BasePanel; 
import net.sf.jabref.BibtexDatabase; 
import net.sf.jabref.Globals; 


public  class  UndoablePreambleChange  extends AbstractUndoableEdit {
	

    private BibtexDatabase base;

	
    private String oldValue, newValue;

	
    private BasePanel panel;

	

    public UndoablePreambleChange(BibtexDatabase base, BasePanel panel,
				  String oldValue, String newValue) {
	this.base = base;
	this.oldValue = oldValue;
	this.newValue = newValue;
	this.panel = panel;
    }


	

    public String getUndoPresentationName() {
	return Globals.lang("Undo")+": "+Globals.lang("change preamble");
    }


	

    public String getRedoPresentationName() {
	return Globals.lang("Redo")+": "+Globals.lang("change preamble");
    }


	

    public void undo() {
	super.undo();
	
	
	base.setPreamble(oldValue);

	
	panel.updatePreamble();
    }


	

    public void redo() {
	super.redo();

	
	base.setPreamble(newValue);

	
	panel.updatePreamble();

    }



}
