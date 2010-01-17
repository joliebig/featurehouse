package net.sf.jabref.export.layout.format; 

import net.sf.jabref.export.layout.ParamLayoutFormatter; 
import net.sf.jabref.export.ExportFormats; 


public  class  Number implements  ParamLayoutFormatter {
	
    public void setArgument(String arg) {
        
    }

	

    public String format(String fieldText) {
        return String.valueOf(ExportFormats.entryNumber);
    }


}
