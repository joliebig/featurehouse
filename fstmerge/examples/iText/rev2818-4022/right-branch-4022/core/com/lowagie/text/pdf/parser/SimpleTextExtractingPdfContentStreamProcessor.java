
package com.lowagie.text.pdf.parser;


public class SimpleTextExtractingPdfContentStreamProcessor extends PdfContentStreamProcessor {

    
    Matrix lastTextLineMatrix = null;
    
    Matrix lastEndingTextMatrix = null;

    
    StringBuffer result = null;

    
    public SimpleTextExtractingPdfContentStreamProcessor() {
    }

    public void reset() {
        super.reset();
        lastTextLineMatrix = null;
        lastEndingTextMatrix = null;
        result = new StringBuffer();
    }
    
    
    public String getResultantText(){
        return result.toString();
    }
    
    
    public void displayText(String text, Matrix endingTextMatrix){
        boolean hardReturn = false;
        if (lastTextLineMatrix != null && lastTextLineMatrix.get(Matrix.I32) != getCurrentTextLineMatrix().get(Matrix.I32)){
        
            hardReturn = true;
        }

        float currentX = getCurrentTextMatrix().get(Matrix.I31);
        if (hardReturn){
            
            result.append('\n');
        } else if (lastEndingTextMatrix != null){
            float lastEndX = lastEndingTextMatrix.get(Matrix.I31);
            
            
            
            float spaceGlyphWidth = gs().font.getWidth(' ')/1000f;
            float spaceWidth = (spaceGlyphWidth * gs().fontSize + gs().characterSpacing + gs().wordSpacing) * gs().horizontalScaling; 
            Matrix scaled = new Matrix(spaceWidth, 0).multiply(getCurrentTextMatrix());
            float scaledSpaceWidth = scaled.get(Matrix.I31) - getCurrentTextMatrix().get(Matrix.I31);
            
            if (currentX - lastEndX > scaledSpaceWidth/2f ){
                
                result.append(' ');
            }
        } else {
            
        }
        
        
        
        result.append(text);

        lastTextLineMatrix = getCurrentTextLineMatrix();
        lastEndingTextMatrix = endingTextMatrix;
        
    }

}
