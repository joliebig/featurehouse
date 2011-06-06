

package com.lowagie.text;



public interface LargeElement extends Element {
    
    
    public void setComplete(boolean complete);
    
    
    public boolean isComplete();
    
    
    public void flushContent();
}
