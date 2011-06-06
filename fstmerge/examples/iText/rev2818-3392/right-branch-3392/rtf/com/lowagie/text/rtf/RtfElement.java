

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.rtf.document.RtfDocument;


public abstract class RtfElement implements RtfBasicElement {
    
    protected RtfDocument document = null;
    
    protected boolean inTable = false;
    
    protected boolean inHeader = false;
    
    
    public RtfElement(RtfDocument doc) {
        super();
        this.document = doc;
    }

    
    public byte[] intToByteArray(int i)
    {
        return Integer.toString(i).getBytes();
    }

        
    public abstract void writeContent(final OutputStream out) throws IOException;
    
    
    public void setRtfDocument(RtfDocument doc) {
        this.document = doc;
    }
    
    
    public boolean isInTable() {
        return inTable;
    }
    
    
    public void setInTable(boolean inTable) {
        this.inTable = inTable;
    }
    
    
    public void setInHeader(boolean inHeader) {
        this.inHeader = inHeader;
    }
    
    
}
