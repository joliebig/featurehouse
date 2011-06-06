
 
package com.lowagie.text.rtf.parser.destinations;

import com.lowagie.text.rtf.parser.RtfParser;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordData;


public final class RtfDestinationNull extends RtfDestination {
    private static RtfDestinationNull instance = null;
    private static Object lock = new Object();
    
    private RtfDestinationNull() {
        super();
    }
    
    private RtfDestinationNull(RtfParser parser) {
        super(null);
    }
    
    static public RtfDestinationNull getInstance() {
        synchronized(lock)
        {
            if(instance == null)
                instance = new RtfDestinationNull();
            return instance;
        }
    }
    
    public boolean handleOpeningSubGroup() {
        return true;
    }
    
    
    public void setToDefaults() {
    }

    
    
    
    public boolean closeDestination() {
        return true;
    }
    
    public boolean handleCloseGroup() {
        
        return true;
    }

    
    public boolean handleOpenGroup() {
        
        return true;
    }
    
    public boolean handleCharacter(int ch) {
        return true;
    }
    
    public boolean handleControlWord(RtfCtrlWordData ctrlWordData) {
        return true;
    }
    
    public static String getName() {
        return RtfDestinationNull.class.getName();
    }
    
    public int getNewTokeniserState() {
        return RtfParser.TOKENISER_SKIP_GROUP;
    }

}
