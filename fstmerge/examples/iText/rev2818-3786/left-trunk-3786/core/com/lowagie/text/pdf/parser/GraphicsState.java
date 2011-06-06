
package com.lowagie.text.pdf.parser;

import com.lowagie.text.pdf.CMapAwareDocumentFont;


public class GraphicsState {
    
    Matrix ctm;
    
    float characterSpacing;
    
    float wordSpacing;
    
    float horizontalScaling;
    
    float leading;
    
    CMapAwareDocumentFont font;
    
    float fontSize;
    
    int renderMode;
    
    float rise;
    
    boolean knockout;
    
    
    public GraphicsState(){
        ctm = new Matrix();
        characterSpacing = 0;
        wordSpacing = 0;
        horizontalScaling = 1.0f;
        leading = 0;
        font = null;
        fontSize = 0;
        renderMode = 0;
        rise = 0;
        knockout = true;
    }
    
    
    public GraphicsState(GraphicsState source){
        
        
        ctm = source.ctm;
        characterSpacing = source.characterSpacing;
        wordSpacing = source.wordSpacing;
        horizontalScaling = source.horizontalScaling;
        leading = source.leading;
        font = source.font;
        fontSize = source.fontSize;
        renderMode = source.renderMode;
        rise = source.rise;
        knockout = source.knockout;
    }
}
