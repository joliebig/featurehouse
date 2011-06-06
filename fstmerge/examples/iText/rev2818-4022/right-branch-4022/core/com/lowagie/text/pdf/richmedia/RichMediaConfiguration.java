

package com.lowagie.text.pdf.richmedia;

import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfString;


public class RichMediaConfiguration extends PdfDictionary {

    
    protected PdfArray instances = new PdfArray();
    
    
    public RichMediaConfiguration(PdfName subtype) {
        super(PdfName.RICHMEDIACONFIGURATION);
        put(PdfName.SUBTYPE, subtype);
        put(PdfName.INSTANCES, instances);
    }
    
    
    public void setName(PdfString name) {
        put(PdfName.NAME, name);
    }
    
    
    public void addInstance(RichMediaInstance instance) {
        instances.add(instance);
    }
}
