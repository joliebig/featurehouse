





















package net.sf.jabref.export.layout.format;

import net.sf.jabref.export.layout.LayoutFormatter;
import java.util.regex.*;
import java.util.Iterator;
import net.sf.jabref.Util;
import net.sf.jabref.Globals;


public class XMLChars implements LayoutFormatter
{
    
    
    Pattern pattern = Pattern.compile(".*\\{\\\\.*[a-zA-Z]\\}.*");
  
    public String format(String fieldText)
    {
 
	fieldText = firstFormat(fieldText);

	
	
        
	for (Iterator i=Globals.HTML_CHARS.keySet().iterator(); i.hasNext();) {
	    String s = (String)i.next();         
            String repl = (String)Globals.XML_CHARS.get(s);
            if (repl != null)
                fieldText = fieldText.replaceAll(s, repl);
	}
	
	return restFormat(fieldText);
    }

    private String firstFormat(String s) {
	return s.replaceAll("&|\\\\&","&#x0026;").replaceAll("--", "&#x2013;");
    }

    private String restFormat(String s) {
		String fieldText=s.replaceAll("\\}","").replaceAll("\\{","");
		
		
		
		int code;
		char character;
		StringBuffer buffer=new StringBuffer(fieldText.length()<<1);
    for ( int i = 0; i < fieldText.length(); i++)
    {
    	character = fieldText.charAt(i);
      code = ((int) character);
      
      if((code<40 && code!=32)||code>125){
      	buffer.append("&#" + code+";");
      }
      else 
      {
      	
      	int[] forceReplace=new int[]{44,45,63,64,94,95,96,124};
      	boolean alphabet=true;
      	for(int ii=0;ii<forceReplace.length;ii++){
      		if(code==forceReplace[ii]){
      			buffer.append("&#" + code+";");
      			alphabet=false;
      			break;
      		}
      	}
    		
      	if(alphabet)buffer.append((char)code);
      }
    }
    fieldText=buffer.toString();

		
		for (Iterator i=Globals.ASCII2XML_CHARS.keySet().iterator(); i.hasNext();) {
	    String ss = (String)i.next();         
            String repl = (String)Globals.ASCII2XML_CHARS.get(ss);
            if (repl != null)
                fieldText = fieldText.replaceAll(ss, repl);
	  }
		
		return fieldText;
    }
}



