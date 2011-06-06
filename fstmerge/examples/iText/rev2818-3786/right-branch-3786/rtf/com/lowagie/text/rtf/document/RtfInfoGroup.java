

package com.lowagie.text.rtf.document;

import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

import com.lowagie.text.DocWriter;
import com.lowagie.text.rtf.RtfElement;



public class RtfInfoGroup extends RtfElement {
    
    private static final byte[] INFO_GROUP = DocWriter.getISOBytes("\\info");
    
    
    private static final byte[] INFO_PASSWORD = DocWriter.getISOBytes("\\*\\password");

    
    ArrayList<RtfInfoElement> infoElements = null;
    
    
    public RtfInfoGroup(RtfDocument doc) {
        super(doc);
        infoElements = new ArrayList<RtfInfoElement>();
    }
    
    
    public void add(RtfInfoElement infoElement) {
        this.infoElements.add(infoElement);
    }
    
        
    public void writeContent(final OutputStream result) throws IOException
    {
        result.write(OPEN_GROUP);
        result.write(INFO_GROUP);
        for(int i = 0; i < infoElements.size(); i++) {
            RtfInfoElement infoElement = infoElements.get(i);
            infoElement.writeContent(result);
        }
        
        
        if(document.getDocumentSettings().isDocumentProtected()) {
            result.write(OPEN_GROUP);
            result.write(INFO_PASSWORD);
            result.write(DELIMITER);
            result.write(document.getDocumentSettings().getProtectionHashBytes());
            result.write(CLOSE_GROUP);
        }

        result.write(CLOSE_GROUP);
        this.document.outputDebugLinebreak(result);
    }        
    
}
