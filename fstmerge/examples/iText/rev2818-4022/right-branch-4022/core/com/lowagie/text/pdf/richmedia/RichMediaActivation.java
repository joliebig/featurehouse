

package com.lowagie.text.pdf.richmedia;

import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfIndirectReference;
import com.lowagie.text.pdf.PdfName;


public class RichMediaActivation extends PdfDictionary {
    
    
    public RichMediaActivation() {
        super(PdfName.RICHMEDIAACTIVATION);
    }
    
    
    public void setCondition(PdfName condition) {
        put(PdfName.CONDITION, condition);
    }
    
    
    public void setAnimation(RichMediaAnimation animation) {
        put(PdfName.ANIMATION, animation);
    }
    
    
    public void setView(PdfIndirectReference view) {
        put(PdfName.VIEW, view);
    }
    
    
    public void setConfiguration(PdfIndirectReference configuration) {
        put(PdfName.CONFIGURATION, configuration);
    }
    
    
    public void setPresentation(RichMediaPresentation richMediaPresentation) {
        put(PdfName.PRESENTATION, richMediaPresentation);
    }
    
    
    public void setScripts(PdfArray scripts) {
        put(PdfName.SCRIPTS, scripts);
    }
}
