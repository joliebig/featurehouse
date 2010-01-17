package net.sf.jabref.export.layout; 

import java.util.ArrayList; 
import java.util.List; 


public abstract  class  AbstractParamLayoutFormatter implements  ParamLayoutFormatter {
	

    private static char SEPARATOR = ',';

	

    
    public static String[] parseArgument(String arg) {
        List<String> parts = new ArrayList<String>();
        StringBuilder current = new StringBuilder();
        boolean escaped = false;
        for (int i=0; i<arg.length(); i++) {
            if ((arg.charAt(i) == SEPARATOR) && !escaped) {
                parts.add(current.toString());
                current = new StringBuilder();
            } else if (arg.charAt(i) == '\\') {
                if (escaped) {
                    escaped = false;
                    current.append(arg.charAt(i));
                } else
                    escaped = true;
            } else if (escaped) {
                
                if (arg.charAt(i)=='n')
                    current.append('\n');
                else if (arg.charAt(i)=='t')
                    current.append('\t');
                else {
                    if ((arg.charAt(i) != ',') && (arg.charAt(i) != '"'))
                        current.append('\\');
                    current.append(arg.charAt(i));
                }
                escaped = false;
            } else
                current.append(arg.charAt(i));
        }
        parts.add(current.toString());
	    return parts.toArray(new String[parts.size()]);
    }



}
