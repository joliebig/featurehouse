

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Chunk;
import com.lowagie.text.DocWriter;
import com.lowagie.text.Font;
import com.lowagie.text.rtf.document.RtfDocument;


public abstract class RtfAddableElement extends Chunk implements RtfBasicElement {

    
    protected RtfDocument doc = null;
    
    protected boolean inTable = false;
    
    protected boolean inHeader = false;
    
    
    public RtfAddableElement() {
        super("", new Font());
    }

    
    public abstract void writeContent(OutputStream out) throws IOException;
    
    
    public void setRtfDocument(RtfDocument doc) {
        this.doc = doc;
    }

    
    public void setInTable(boolean inTable) {
        this.inTable = inTable;
    }

    
    public void setInHeader(boolean inHeader) {
        this.inHeader = inHeader;
    }

    
    public byte[] intToByteArray(int i) {
        return DocWriter.getISOBytes(Integer.toString(i));
    }
    
    
    public boolean isEmpty() {
        return false;
    }
}
