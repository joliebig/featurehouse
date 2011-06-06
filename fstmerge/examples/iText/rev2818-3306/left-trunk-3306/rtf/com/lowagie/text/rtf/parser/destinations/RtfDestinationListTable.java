
 
package com.lowagie.text.rtf.parser.destinations;

import com.lowagie.text.rtf.parser.RtfImportMgr;
import com.lowagie.text.rtf.parser.RtfParser;
import com.lowagie.text.rtf.parser.ctrlwords.RtfCtrlWordData;


public class RtfDestinationListTable extends RtfDestination {
    
    private RtfImportMgr importHeader = null;
    
    public RtfDestinationListTable() {
        super(null);
    }
    
    public RtfDestinationListTable(RtfParser parser) {
        super(parser);
        this.importHeader = parser.getImportManager();
    }
    
    public void setParser(RtfParser parser) {
        this.rtfParser = parser;
        this.importHeader = parser.getImportManager();
        this.setToDefaults();
    }
    
    public boolean handleOpeningSubGroup() {
        return true;
    }
    
    public boolean closeDestination() {
        
        return true;
    }
    public boolean handleControlWord(RtfCtrlWordData ctrlWordData) {
        boolean result = true;
        return result;
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

    
    public void setToDefaults() {
        
        
    }

}
