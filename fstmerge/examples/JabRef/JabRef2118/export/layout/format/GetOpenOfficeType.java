





















package net.sf.jabref.export.layout.format; 

import net.sf.jabref.export.layout.LayoutFormatter; 



public  class  GetOpenOfficeType implements  LayoutFormatter {
	
    

    public String format(String fieldText)
    {
        String fieldEntry = fieldText;
		if (fieldEntry.equalsIgnoreCase("Article")) return "7";
		if (fieldEntry.equalsIgnoreCase("Book")) return "1";
		if (fieldEntry.equalsIgnoreCase("Booklet")) return "2";
		if (fieldEntry.equalsIgnoreCase("Inbook")) return "5";
		if (fieldEntry.equalsIgnoreCase("Incollection")) return "5";
		if (fieldEntry.equalsIgnoreCase("Inproceedings")) return "6";
		if (fieldEntry.equalsIgnoreCase("Manual")) return "8";
		if (fieldEntry.equalsIgnoreCase("Mastersthesis")) return "9";
		if (fieldEntry.equalsIgnoreCase("Misc")) return "10";
		if (fieldEntry.equalsIgnoreCase("Other")) return "10";
		if (fieldEntry.equalsIgnoreCase("Phdthesis")) return "9";
		if (fieldEntry.equalsIgnoreCase("Proceedings")) return "3";
		if (fieldEntry.equalsIgnoreCase("Techreport")) return "13";
		if (fieldEntry.equalsIgnoreCase("Unpublished")) return "14";
	
		return "10";
    }



}
