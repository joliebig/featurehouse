

package com.lowagie.text.pdf.richmedia;

import com.lowagie.text.pdf.PdfAction;
import com.lowagie.text.pdf.PdfIndirectReference;
import com.lowagie.text.pdf.PdfName;


public class RichMediaExecuteAction extends PdfAction {

    
    public RichMediaExecuteAction(PdfIndirectReference ref,
            RichMediaCommand command) {
        super();
        put(PdfName.S, PdfName.RICHMEDIAEXECUTE);
        put(PdfName.TA, ref);
        put(PdfName.CMD, command);
    }
    
    
    public void setRichMediaInstance(PdfIndirectReference ref) {
        put(PdfName.TI, ref);
    }
}
