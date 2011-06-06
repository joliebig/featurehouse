

package com.lowagie.text.rtf.document;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

import com.lowagie.text.rtf.RtfElement;



public class RtfInfoGroup extends RtfElement {
    
    private static final byte[] INFO_GROUP = "\\info".getBytes();
    
    
    ArrayList<RtfInfoElement> infoElements = null;
    
    
    public RtfInfoGroup(RtfDocument doc) {
        super(doc);
        infoElements = new ArrayList<RtfInfoElement>();
    }
    
    
    public void add(RtfInfoElement infoElement) {
        this.infoElements.add(infoElement);
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
        result.write(INFO_GROUP);
        for(int i = 0; i < infoElements.size(); i++) {
            RtfInfoElement infoElement = infoElements.get(i);
            
            infoElement.writeContent(result);
        }
        result.write(CLOSE_GROUP);
        result.write((byte) '\n');
    }        
    
}
