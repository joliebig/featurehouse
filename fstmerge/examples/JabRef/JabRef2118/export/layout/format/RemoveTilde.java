





















package net.sf.jabref.export.layout.format; 

import net.sf.jabref.export.layout.LayoutFormatter; 


public  class  RemoveTilde implements  LayoutFormatter {
	

	public String format(String fieldText) {
		
		StringBuffer result = new StringBuffer(fieldText.length());

		char[] c = fieldText.toCharArray();
		
		for (int i = 0; i < c.length; i++) {

			if (c[i] != '~'){
				result.append(c[i]);
				
				if (c[i] == '\\' && i + 1 < c.length){
					i++;
					result.append(c[i]);
				}
			} else {
				result.append(' ');
			}
		}
		
		return result.toString();
	}



}
