
package net.sf.jabref.imports;


public class BooleanAssign {
    
    boolean value;
    
    
	public BooleanAssign(boolean b) {
		setValue(b);		
	}

	public void setValue(boolean value) {
        this.value = value;
    }
    
    public boolean getValue() {
        return(value);
    }
}
