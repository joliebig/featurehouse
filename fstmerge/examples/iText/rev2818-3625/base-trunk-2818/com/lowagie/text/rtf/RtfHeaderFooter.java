
package com.lowagie.text.rtf;

import com.lowagie.text.Element;
import com.lowagie.text.HeaderFooter;
import com.lowagie.text.Phrase;



public class RtfHeaderFooter extends HeaderFooter {


    private Element content = null;

        
    
    public RtfHeaderFooter( Phrase before, Phrase after ) {
        super( before, after );    
    }    


    
    public RtfHeaderFooter( Phrase before, boolean numbered ) {
        super( before, numbered );
    }    


    
    public RtfHeaderFooter( Element content ) {
        super(new Phrase(content.toString()), false);
        this.content = content;
    }    


    
    public Element content() {
        return content;
    }
}
