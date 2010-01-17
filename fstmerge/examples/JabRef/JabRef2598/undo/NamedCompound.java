

package net.sf.jabref.undo; 

import javax.swing.undo.CompoundEdit; 
import javax.swing.undo.UndoableEdit; 

import net.sf.jabref.Globals; 

public  class  NamedCompound  extends CompoundEdit {
	

    String name;

	
    boolean hasEdits = false;

	

    public NamedCompound(String name) {
	super();
	this.name = name;
    }


	

    public boolean addEdit(UndoableEdit undoableEdit) {
        hasEdits = true;
        return super.addEdit(undoableEdit);
    }


	

    public boolean hasEdits() {
        return hasEdits;
    }


	

    public String getUndoPresentationName() {
	return Globals.lang("Undo")+": "+name;
    }


	

    public String getRedoPresentationName() {
	return Globals.lang("Redo")+": "+name;
    }


	

    
    public String getNameOnly() {
      return name;
    }



}
