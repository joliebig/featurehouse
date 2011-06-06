

package com.lowagie.text.rtf.document;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfExtendedElement;


public class RtfCodePage extends RtfElement implements RtfExtendedElement {
    
    private static final byte[] ANSI = "\\ansi".getBytes();
    
    private static final byte[] ANSI_CODEPAGE = "\\ansicpg".getBytes();

    
    public RtfCodePage(RtfDocument doc) {
        super(doc);
    }

    
    public void writeContent(final OutputStream out) throws IOException
    {        
    }
    
    
    public void writeDefinition(final OutputStream result) throws IOException
    {
        result.write(ANSI);
        result.write(ANSI_CODEPAGE);
        result.write(intToByteArray(1252));
        result.write((byte)'\n');        
    }

}
