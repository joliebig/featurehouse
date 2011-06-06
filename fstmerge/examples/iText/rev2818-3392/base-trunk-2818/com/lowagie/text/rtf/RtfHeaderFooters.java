

package com.lowagie.text.rtf;

import com.lowagie.text.HeaderFooter;
import com.lowagie.text.Phrase;



public class RtfHeaderFooters extends HeaderFooter {
    
    public final static int ALL_PAGES = 0;
    
    public final static int LEFT_PAGES = 1;
    
    public final static int RIGHT_PAGES = 2;
    
    public final static int FIRST_PAGE = 3;
    


    
    private HeaderFooter allPages = null;
    
    private HeaderFooter leftPages = null;
    
    private HeaderFooter rightPages = null;
    
    private HeaderFooter firstPage = null;

    
    public RtfHeaderFooters() {
        super( new Phrase(""), false );
    }

    
    public RtfHeaderFooters( Phrase before, Phrase after ) {
        super( before, after );
    }

    
    public RtfHeaderFooters( Phrase before, boolean numbered ) {
        super( before, numbered );
    }

    
    public void set( int type, HeaderFooter hf ) {
        switch (type) {
            case ALL_PAGES:
                allPages = hf;
                break;
            case LEFT_PAGES:
                leftPages = hf;
                break;
            case RIGHT_PAGES:
                rightPages = hf;
                break;
            case FIRST_PAGE:
                firstPage = hf;
                break;
            default:
                throw new IllegalStateException( "unknown type " + type );
        }
    }

    
    public HeaderFooter get( int type ) {
        switch (type) {
            case ALL_PAGES:
                return allPages;
            case LEFT_PAGES:
                return leftPages;
            case RIGHT_PAGES:
                return rightPages;
            case FIRST_PAGE:
                return firstPage;
            default:
                throw new IllegalStateException( "unknown type " + type );
        }
    }    
}
