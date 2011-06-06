

package com.lowagie.text.rtf.text;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.Chapter;
import com.lowagie.text.rtf.RtfBasicElement;
import com.lowagie.text.rtf.document.RtfDocument;



public class RtfChapter extends RtfSection {

    
    public RtfChapter(RtfDocument doc, Chapter chapter) {
        super(doc, chapter);
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
        if(this.document.getLastElementWritten() != null && !(this.document.getLastElementWritten() instanceof RtfChapter)) {
            result.write("\\page".getBytes());
        }
        result.write("\\sectd".getBytes());
        
        document.getDocumentHeader().writeSectionDefinition(result);
        if(this.title != null) {
            
            this.title.writeContent(result);
        }
        for(int i = 0; i < items.size(); i++) {
            RtfBasicElement rbe = items.get(i);
            
            rbe.writeContent(result);
        }
        result.write("\\sect".getBytes());
    }        
    
}
