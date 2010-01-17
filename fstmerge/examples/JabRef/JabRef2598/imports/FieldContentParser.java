package net.sf.jabref.imports; 

import net.sf.jabref.Globals; 
import net.sf.jabref.GUIGlobals; 



public  class  FieldContentParser {
	

    
    public StringBuffer format(StringBuffer content) { 
    	return format(content, null);
    }


	

    
    public static String wrap(String in, int wrapAmount){
        
        String[] lines = in.split("\n");
        StringBuffer res = new StringBuffer();
        addWrappedLine(res, lines[0], wrapAmount);
        for (int i=1; i<lines.length; i++) {

            if (!lines[i].trim().equals("")) {
                res.append(Globals.NEWLINE);
                res.append('\t');
                res.append(Globals.NEWLINE);
                res.append('\t');
                addWrappedLine(res, lines[i], wrapAmount);
            } else {
                res.append(Globals.NEWLINE);
                res.append('\t');
            }
        }
        return res.toString();
    }


	

    private static void addWrappedLine(StringBuffer res, String line, int wrapAmount) {
        
        int p = res.length();
        
        res.append(line);

        while (p < res.length()){
            int q = res.indexOf(" ", p+wrapAmount);
            if ((q < 0) || (q >= res.length()))
                break;

            res.deleteCharAt(q);
            res.insert(q, Globals.NEWLINE+"\t");
            p = q+Globals.NEWLINE_LENGTH;

        }
    }


	

    static  class  Indents {

	}

	

    
	public StringBuffer format(StringBuffer content, String key) {

        
        

        int i=0;

        
        
        
        content = new StringBuffer(content.toString().replaceAll("\r\n","\n").replaceAll("\r", "\n"));

        while (i<content.length()) {

            int c = content.charAt(i);
            if (c == '\n') {
                if ((content.length()>i+1) && (content.charAt(i+1)=='\t')
                    && ((content.length()==i+2) || !Character.isWhitespace(content.charAt(i+2)))) {
                    
                    

                    content.deleteCharAt(i); 
                    content.deleteCharAt(i); 
                    
                    
                    
                    
                    
                    
                    if ((i==0) || !Character.isWhitespace(content.charAt(i-1))) {
                        content.insert(i, ' ');
                        
                        i++;
                    }
                }
                else if ((content.length()>i+3) && (content.charAt(i+1)=='\t')
                    && (content.charAt(i+2)==' ')
                    && !Character.isWhitespace(content.charAt(i+3))) {
                    
                    
                    content.deleteCharAt(i); 
                    content.deleteCharAt(i); 
                    
                    if ((i>0) && Character.isWhitespace(content.charAt(i-1))) {
                        content.deleteCharAt(i);
                    }
                }
                else if ((content.length()>i+3) && (content.charAt(i+1)=='\t')
                        && (content.charAt(i+2)=='\n') && (content.charAt(i+3)=='\t')) {
                    
                    
                    content.deleteCharAt(i+1); 
                    content.deleteCharAt(i+1); 
                    content.deleteCharAt(i+1); 
                    
                    i++;

                    
                    
                    while ((content.length()>i+1) && (content.charAt(i)=='\n')
                        && (content.charAt(i+1)=='\t')) {

                        content.deleteCharAt(i+1);
                        i++;
                    }
                }
                else if ((content.length()>i+1) && (content.charAt(i+1)!='\n')) {
                    
                    
                    content.deleteCharAt(i);
                    
                    if (!Character.isWhitespace(content.charAt(i)) &&  
                            (i>0) && !Character.isWhitespace(content.charAt(i-1))) 
                        content.insert(i, ' ');
                }

                
                else
                    i++;
                
            }
            else if (c == ' ') {
                
                if ((i>0) && (content.charAt(i-1)==' ')) {
                    
                	
                	
                	
                	if(key != null && key.equals(GUIGlobals.FILE_FIELD)){
                		i++;
                	}
                	else
                		content.deleteCharAt(i);
                }
                else
                    i++;
            } else if (c == '\t')
                
                content.deleteCharAt(i);
            else
                i++;

        }
        
        return content;
	}


}
