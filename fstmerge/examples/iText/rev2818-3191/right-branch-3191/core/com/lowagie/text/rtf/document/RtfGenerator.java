

package com.lowagie.text.rtf.document;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Document;
import com.lowagie.text.rtf.RtfElement;



public class RtfGenerator extends RtfElement {
    
    private static final byte[] GENERATOR = "\\*\\generator".getBytes();
    
    
    public RtfGenerator(RtfDocument doc) {
        super(doc);
    }
    
    
    
    public byte[] write()
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            writeContent(result);
        } catch(IOException e) {
            e.printStackTrace();
        }
        return result.toByteArray();
    }
    
        
    public void writeContent(final OutputStream result) throws IOException
    {
        result.write(OPEN_GROUP);
        result.write(GENERATOR);
        result.write(DELIMITER);
        result.write(Document.getVersion().getBytes());
        result.write(CLOSE_GROUP);
        result.write((byte) '\n');
    }        
    
}
