

package com.lowagie.text.pdf;


public interface HyphenationEvent {

        
    public String getHyphenSymbol();
    
        
    public String getHyphenatedWordPre(String word, BaseFont font, float fontSize, float remainingWidth);
    
        
    public String getHyphenatedWordPost();
}

