package net.sf.jabref.collab; 

import net.sf.jabref.Globals; 
import net.sf.jabref.BasePanel; 
import javax.swing.tree.DefaultMutableTreeNode; 
import javax.swing.JComponent; 
import net.sf.jabref.undo.NamedCompound; 

public abstract  class  Change  extends DefaultMutableTreeNode {
	

  String name;

	
  boolean accepted = true;

	

  public Change() {
    name = "";
  }


	

  public Change(String name) {
    this.name = Globals.lang(name);
  }


	

  public String getName() {
    return name;
  }


	

  public String toString() {
    return getName();
  }


	

  public boolean isAccepted() {
    return accepted;
  }


	

  public void setAccepted(boolean a) {
    accepted = a;
  }


	

  
  public boolean isAcceptable() {
    if ((getParent() != null) && (getParent() instanceof Change))
      return ((Change)getParent()).isAccepted();
    else
      return true;
  }


	

  
  abstract JComponent description();


	

  
  abstract void makeChange(BasePanel panel, NamedCompound undoEdit);



}
