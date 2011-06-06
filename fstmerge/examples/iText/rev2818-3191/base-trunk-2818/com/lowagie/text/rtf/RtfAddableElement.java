

package com.lowagie.text.rtf;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Chunk;
import com.lowagie.text.Font;
import com.lowagie.text.rtf.document.RtfDocument;


public abstract class RtfAddableElement extends Chunk implements RtfBasicElement {

    
    protected RtfDocument doc = null;
    
    protected boolean inTable = false;
    
    protected boolean inHeader = false;
    
    
    public RtfAddableElement() {
        super("", new Font());
    }

    
    public abstract byte[] write();

    
    public void writeContent(OutputStream out) throws IOException
    {
        byte[] content = write();
        out.write(content);
    }
    
    
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
        return Integer.toString(i).getBytes();
    }
    
    
    public boolean isEmpty() {
        return false;
    }
}
