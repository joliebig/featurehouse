

package com.lowagie.text.pdf;


public interface ExtraEncoding {
    
        
    public byte[] charToByte(String text, String encoding);
    
        
    public byte[] charToByte(char char1, String encoding);
    
    
    public String byteToChar(byte b[], String encoding);   
}