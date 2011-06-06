
package com.lowagie.text.pdf;

import java.io.IOException;

public class PdfShadingPattern extends PdfDictionary {

    protected PdfShading shading;
    
    protected PdfWriter writer;
    
    protected float matrix[] = {1, 0, 0, 1, 0, 0};
    
    protected PdfName patternName;

    protected PdfIndirectReference patternReference;

    
    public PdfShadingPattern(PdfShading shading) {
        writer = shading.getWriter();
        put(PdfName.PATTERNTYPE, new PdfNumber(2));
        this.shading = shading;
    }
        
    PdfName getPatternName() {
        return patternName;
    }

    PdfName getShadingName() {
        return shading.getShadingName();
    }
    
    PdfIndirectReference getPatternReference() {
        if (patternReference == null)
            patternReference = writer.getPdfIndirectReference();
        return patternReference;
    }
    
    PdfIndirectReference getShadingReference() {
        return shading.getShadingReference();
    }
    
    void setName(int number) {
        patternName = new PdfName("P" + number);
    }
    
    void addToBody() throws IOException {
        put(PdfName.SHADING, getShadingReference());
        put(PdfName.MATRIX, new PdfArray(matrix));
        writer.addToBody(this, getPatternReference());
    }
    
    public void setMatrix(float matrix[]) {
        if (matrix.length != 6)
            throw new RuntimeException("The matrix size must be 6.");
        this.matrix = matrix;
    }
    
    public float[] getMatrix() {
        return matrix;
    }
    
    public PdfShading getShading() {
        return shading;
    }
    
    ColorDetails getColorDetails() {
        return shading.getColorDetails();
    }

}
