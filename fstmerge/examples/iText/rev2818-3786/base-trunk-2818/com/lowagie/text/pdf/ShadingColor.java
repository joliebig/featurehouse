

package com.lowagie.text.pdf;


public class ShadingColor extends ExtendedColor {

    private static final long serialVersionUID = 4817929454941328671L;
    PdfShadingPattern shadingPattern;

    
    public ShadingColor(PdfShadingPattern shadingPattern) {
        super(TYPE_SHADING, .5f, .5f, .5f);
        this.shadingPattern = shadingPattern;
    }

    
    public PdfShadingPattern getPdfShadingPattern() {
        return shadingPattern;
    }
    
    public boolean equals(Object obj) {
        return this == obj;
    }
    
    public int hashCode() {
        return shadingPattern.hashCode();
    }
    
}
