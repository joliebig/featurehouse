

package com.lowagie.text.pdf.hyphenation;

import java.util.ArrayList;


public interface PatternConsumer {

    
    void addClass(String chargroup);

    
    void addException(String word, ArrayList<Object> hyphenatedword);

    
    void addPattern(String pattern, String values);

}
