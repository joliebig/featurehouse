package com.lowagie.text.pdf.collection;

import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNumber;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfString;

public class PdfTargetDictionary extends PdfDictionary {
    
    
    public PdfTargetDictionary(PdfTargetDictionary nested) {
        super();
        put(PdfName.R, PdfName.P);
        if (nested != null)
            setAdditionalPath(nested);
    }
    
    
    public PdfTargetDictionary(boolean child) {
        super();
        if (child) {
            put(PdfName.R, PdfName.C);
        }
        else {
            put(PdfName.R, PdfName.P);
        }
    }
    
    
    public void setEmbeddedFileName(String target) {
        put(PdfName.N, new PdfString(target, null));
    }
    
    
    public void setFileAttachmentPagename(String name) {
        put(PdfName.P, new PdfString(name, null));
    }
    
    
    public void setFileAttachmentPage(int page) {
        put(PdfName.P, new PdfNumber(page));
    }
    
    
    public void setFileAttachmentName(String name) {
        put(PdfName.A, new PdfString(name, PdfObject.TEXT_UNICODE));
    }
    
    
    public void setFileAttachmentIndex(int annotation) {
        put(PdfName.A, new PdfNumber(annotation));
    }
    
    
    public void setAdditionalPath(PdfTargetDictionary nested) {
        put(PdfName.T, nested);
    }
}
