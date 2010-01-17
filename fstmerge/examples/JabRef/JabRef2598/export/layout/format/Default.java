package net.sf.jabref.export.layout.format; 

import net.sf.jabref.export.layout.ParamLayoutFormatter; 


public  class  Default implements  ParamLayoutFormatter {
	

    String defValue = "";

	

    public void setArgument(String arg) {
        this.defValue = arg;
    }

	

    public String format(String fieldText) {
        return fieldText != null && (fieldText.length() > 0) ? fieldText : defValue;
    }


}
