

package com.lowagie.text.rtf.field;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Font;



public class RtfTOCEntry extends RtfField {

    
    private static final byte[] TEXT_HIDDEN_ON = "\\v".getBytes();
    
    private static final byte[] TEXT_HIDDEN_OFF = "\\v0".getBytes();
    
    private static final byte[] TOC_ENTRY_PAGE_NUMBER = "\\tc".getBytes();
    
    private static final byte[] TOC_ENTRY_NO_PAGE_NUMBER = "\\tcn".getBytes();
    
    
    private String entry = "";
    
    private boolean showPageNumber = true;
    
    
    public RtfTOCEntry(String entry) {
        super(null, new Font());
        if(entry != null) {
            this.entry = entry;
        }
    }
    
    
    public byte[] write() {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            writeContent(result);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }
     
    public void writeContent(final OutputStream result) throws IOException
    {        
        result.write(TEXT_HIDDEN_ON);
        result.write(OPEN_GROUP);
        if(this.showPageNumber) {
            result.write(TOC_ENTRY_PAGE_NUMBER);
        } else {
            result.write(TOC_ENTRY_NO_PAGE_NUMBER);
        }
        result.write(DELIMITER);
        
        this.document.filterSpecialChar(result, this.entry, true, false);
        result.write(CLOSE_GROUP);
        result.write(TEXT_HIDDEN_OFF);
    }
    
    
    public void setShowPageNumber(boolean showPageNumber) {
        this.showPageNumber = showPageNumber;
    }
    
    
    protected byte[] writeFieldInstContent() throws IOException 
    {
        return null;
    }
    
    protected void writeFieldInstContent(OutputStream out) throws IOException 
    {
    }

    
    protected byte[] writeFieldResultContent() throws IOException {
        return null;
    }
    
    
    protected void writeFieldResultContent(OutputStream out) throws IOException
    {
    }

}
