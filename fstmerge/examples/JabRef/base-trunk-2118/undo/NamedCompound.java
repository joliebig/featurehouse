

package net.sf.jabref.undo;

import javax.swing.undo.*;
import net.sf.jabref.Globals;

public class NamedCompound extends CompoundEdit {

    String name;

    public NamedCompound(String name) {
	super();
	this.name = name;
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
