

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.DocWriter;
import com.lowagie.text.RtfElementInterface;
import com.lowagie.text.rtf.document.RtfDocument;


public interface RtfBasicElement extends RtfElementInterface {
    
    public static final byte[] OPEN_GROUP = DocWriter.getISOBytes("{");
    
    public static final byte[] CLOSE_GROUP = DocWriter.getISOBytes("}");
    
    public static final byte[] DELIMITER = DocWriter.getISOBytes(" ");
    
    public static final byte[] COMMA_DELIMITER = DocWriter.getISOBytes(";");
    
    public static final double TWIPS_FACTOR = 20;

    
    public void writeContent(OutputStream out) throws IOException;
    
    
    public void setRtfDocument(RtfDocument doc);
    
    
    public void setInTable(boolean inTable);
    
    
    public void setInHeader(boolean inHeader);
}
