

package com.lowagie.text.rtf.list;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.DocWriter;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.RtfExtendedElement;
import com.lowagie.text.rtf.document.RtfDocument;



public class RtfPictureList  extends RtfElement implements RtfExtendedElement {
    
    private static final byte[] LIST_LEVEL_PICTURE = DocWriter.getISOBytes("\\*\\listpicture");

    public RtfPictureList(RtfDocument doc) {
        super(doc);
    }
    
    public void writeContent(OutputStream out) throws IOException {
        
        
    }

    
    public void writeDefinition(final OutputStream result) throws IOException {
        
        result.write(OPEN_GROUP);
        result.write(LIST_LEVEL_PICTURE);
        
        result.write(CLOSE_GROUP);
    }

}
