




















package net.sf.jabref.export.layout.format; 

import wsi.ra.tool.WSITools; 

import java.util.Vector; 

import net.sf.jabref.export.layout.LayoutFormatter; 
import net.sf.jabref.imports.*; 




public  class  CreateDocBookEditors  extends CreateDocBookAuthors {
	
    

    public String format(String fieldText)
    {
        

        StringBuffer sb = new StringBuffer(100);
        

        if (fieldText.indexOf(" and ") == -1)
        {
          sb.append("<editor>");
          singleAuthor(sb, fieldText);
          sb.append("</editor>");
        }
        else
        {
            String[] names = fieldText.split(" and ");
            for (int i=0; i<names.length; i++)
            {
              sb.append("<editor>");
              singleAuthor(sb, names[i]);
              sb.append("</editor>");
              if (i < names.length -1)
                sb.append("\n       ");
            }
        }

        fieldText = sb.toString();

        return fieldText;
    }



}
