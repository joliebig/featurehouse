
package net.sf.jabref.export.layout.format; 

import net.sf.jabref.export.layout.LayoutFormatter; 


public  class  AuthorAndsReplacer implements  LayoutFormatter {
	

	
	public String format(String fieldText) {

        if (fieldText == null)
            return null;
        String[] authors = fieldText.split(" and ");
		String s;
	
		switch(authors.length) {
			case 1:
				
				s = authors[0];
			break;
			case 2:
				s = authors[0] + " & " + authors[1];
			break;
			default:
				int i = 0, x = authors.length;
				StringBuffer sb = new StringBuffer();
				
				for(i=0;i<x-2;i++) {
                    sb.append(authors[i]).append("; ");
				}
                sb.append(authors[i]).append(" & ").append(authors[i + 1]);
				s = new String(sb);				
			break;		
		}
		
		return s;
 
	}



}
