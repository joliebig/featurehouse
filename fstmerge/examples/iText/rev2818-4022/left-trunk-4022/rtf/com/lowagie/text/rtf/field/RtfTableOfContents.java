

package com.lowagie.text.rtf.field;

import java.io.IOException;
import java.io.OutputStream;

import com.lowagie.text.DocWriter;
import com.lowagie.text.Font;




public class RtfTableOfContents extends RtfField {

    
    private final static String FIELD_INST = "TOC \\\\f \\\\h \\\\u \\\\o \"1-5\" ";
    
    private String defaultText = "Table of Contents - Click to update";
    
    
    public RtfTableOfContents(String defaultText) {
        super(null, new Font());
        this.defaultText = defaultText;
    }
    
     
    protected void writeFieldInstContent(final OutputStream result) throws IOException 
    {
        result.write(DocWriter.getISOBytes(FIELD_INST));
    }

    
    protected void writeFieldResultContent(final OutputStream out) throws IOException 
    {
        document.filterSpecialChar(out, defaultText, true, true);
    }    
}
