
package com.lowagie.text.pdf;

import java.io.IOException;


public class PdfPSXObject extends PdfTemplate {
    
    
    protected PdfPSXObject() {
        super();
    }
    
    
    public PdfPSXObject(PdfWriter wr) {
        super(wr);
    }

    
    
    PdfStream getFormXObject(int compressionLevel) throws IOException {
        PdfStream s = new PdfStream(content.toByteArray());
        s.put(PdfName.TYPE, PdfName.XOBJECT);
        s.put(PdfName.SUBTYPE, PdfName.PS);
        s.flateCompress(compressionLevel);
        return s;
    }
        
    
    
    public PdfContentByte getDuplicate() {
        PdfPSXObject tpl = new PdfPSXObject();
        tpl.writer = writer;
        tpl.pdf = pdf;
        tpl.thisReference = thisReference;
        tpl.pageResources = pageResources;
        tpl.separator = separator;
        return tpl;
    }
}
