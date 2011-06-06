

package com.lowagie.text;



public interface DocListener extends ElementListener {
    
    
    
    
    
    public void open(); 
    
    
        
    public void close(); 
    
    
        
    public boolean newPage(); 
    
    
        
    public boolean setPageSize(Rectangle pageSize); 
        
    
        
    public boolean setMargins(float marginLeft, float marginRight, float marginTop, float marginBottom);  
        
    
    public boolean setMarginMirroring(boolean marginMirroring); 
    
    
    public boolean setMarginMirroringTopBottom(boolean marginMirroringTopBottom); 
        
    
        
    public void setPageCount(int pageN); 
    
    
        
    public void resetPageCount(); 

    
    
    public void setHeader(HeaderFooter header); 
    
    
    
    public void resetHeader(); 
    
    
    
    public void setFooter(HeaderFooter footer); 
    
    
    
    public void resetFooter(); 

}