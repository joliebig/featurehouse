

package com.lowagie.text.pdf.richmedia;

import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfString;


public class RichMediaParams extends PdfDictionary {

    
    public RichMediaParams() {
        super(PdfName.RICHMEDIAPARAMS);
    }
    
    
    public void setFlashVars(String flashVars) {
        put(PdfName.FLASHVARS, new PdfString(flashVars));
    }
    
    
    public void setBinding(PdfName binding) {
        put(PdfName.BINDING, binding);
    }
    
    
    public void setBindingMaterialName(PdfString bindingMaterialName) {
        put(PdfName.BINDINGMATERIALNAME, bindingMaterialName);
    }
    
    
    public void setCuePoints(PdfArray cuePoints) {
        put(PdfName.CUEPOINTS, cuePoints);
    }
    
    
    public void setSettings(PdfString settings) {
        put(PdfName.SETTINGS, settings);
    }
}
