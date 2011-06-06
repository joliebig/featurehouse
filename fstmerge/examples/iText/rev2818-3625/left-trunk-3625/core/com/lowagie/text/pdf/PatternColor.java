
package com.lowagie.text.pdf;


public class PatternColor extends ExtendedColor {
    private static final long serialVersionUID = -1185448552860615964L;
        
    PdfPatternPainter painter;
    
        
    public PatternColor(PdfPatternPainter painter) {
        super(TYPE_PATTERN, .5f, .5f, .5f);
        this.painter = painter;
    }
    
        
    public PdfPatternPainter getPainter() {
        return this.painter;
    }
    
    public boolean equals(Object obj) {
        return this == obj;
    }
    
    public int hashCode() {
        return painter.hashCode();
    }    
}
