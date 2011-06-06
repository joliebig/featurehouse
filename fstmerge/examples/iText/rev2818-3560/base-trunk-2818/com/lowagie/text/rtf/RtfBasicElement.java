

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.rtf.document.RtfDocument;


public interface RtfBasicElement {
    
    public static final byte[] OPEN_GROUP = "{".getBytes();
    
    public static final byte[] CLOSE_GROUP = "}".getBytes();
    
    public static final byte[] DELIMITER = " ".getBytes();
    
    public static final byte[] COMMA_DELIMITER = ";".getBytes();
    
    public static final double TWIPS_FACTOR = 20;

    
    public byte[] write();
    
    
    public void writeContent(OutputStream out) throws IOException;
    
    
    public void setRtfDocument(RtfDocument doc);
    
    
    public void setInTable(boolean inTable);
    
    
    public void setInHeader(boolean inHeader);
}
