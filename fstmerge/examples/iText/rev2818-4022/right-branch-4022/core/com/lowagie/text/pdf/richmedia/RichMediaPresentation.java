

package com.lowagie.text.pdf.richmedia;

import com.lowagie.text.pdf.PdfBoolean;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;


public class RichMediaPresentation extends PdfDictionary {
    
    
    public RichMediaPresentation() {
        super(PdfName.RICHMEDIAPRESENTATION);
    }
    
    
    public void setStyle(PdfName style) {
        put(PdfName.STYLE, style);
    }
    
    
    public void setWindow(RichMediaWindow window) {
        put(PdfName.WINDOW, window);
    }
    
    
    public void setTransparent(PdfBoolean transparent) {
        put(PdfName.TRANSPARENT, transparent);
    }
    
    
    public void setNavigationPane(PdfBoolean navigationPane) {
        put(PdfName.NAVIGATIONPANE, navigationPane);
    }

    
    public void setToolbar(PdfBoolean toolbar) {
        put(PdfName.TOOLBAR, toolbar);
    }

    
    public void setPassContextClick(PdfBoolean passContextClick) {
        put(PdfName.PASSCONTEXTCLICK, passContextClick);
    }
}
