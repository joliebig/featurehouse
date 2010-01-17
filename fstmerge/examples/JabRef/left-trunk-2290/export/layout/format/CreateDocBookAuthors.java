




















package net.sf.jabref.export.layout.format;

import wsi.ra.tool.WSITools;

import java.util.Vector;

import net.sf.jabref.export.layout.LayoutFormatter;
import net.sf.jabref.AuthorList;



public class CreateDocBookAuthors implements LayoutFormatter
{
    

    public String format(String fieldText)
    {
        
        
        
        

        StringBuffer sb = new StringBuffer(100);

        if (fieldText.indexOf(" and ") == -1)
        {
          sb.append("<author>");
          singleAuthor(sb, fieldText);
          sb.append("</author>");
        }
        else
        {
            String[] names = fieldText.split(" and ");
            for (int i=0; i<names.length; i++)
            {
              sb.append("<author>");
              singleAuthor(sb, names[i]);
              sb.append("</author>");
              if (i < names.length -1)
                sb.append("\n       ");
            }
        }



        fieldText = sb.toString();

        return fieldText;
    }

    
    protected void singleAuthor(StringBuffer sb, String author)
    {
        
        Vector<String> v = new Vector<String>();
        String authorMod = AuthorList.fixAuthor_firstNameFirst(author);

        WSITools.tokenize(v, authorMod, " \n\r");

        if (v.size() == 1)
        {
            sb.append("<surname>");
            sb.append(v.get(0));
            sb.append("</surname>");
        }
        else if (v.size() == 2)
        {
            sb.append("<firstname>");
            sb.append(v.get(0));
            sb.append("</firstname>");
            sb.append("<surname>");
            sb.append(v.get(1));
            sb.append("</surname>");
        }
        else
        {
            sb.append("<firstname>");
            sb.append(v.get(0));
            sb.append("</firstname>");
            sb.append("<othername role=\"mi\">");

            for (int i = 1; i < (v.size() - 1); i++)
            {
                sb.append(v.get(i));

                if (i < (v.size() - 2))
                {
                    sb.append(' ');
                }
            }

            sb.append("</othername>");
            sb.append("<surname>");
            sb.append(v.get(v.size() - 1));
            sb.append("</surname>");
        }
    }
}



