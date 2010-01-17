




















package net.sf.jabref.export.layout.format;

import wsi.ra.tool.WSITools;

import java.util.Vector;

import net.sf.jabref.export.layout.LayoutFormatter;
import net.sf.jabref.AuthorList;



public class CreateDocBookAuthors implements LayoutFormatter
{
    

    static XMLChars xc = new XMLChars();

    public String format(String fieldText)
    {

        StringBuilder sb = new StringBuilder(100);

        AuthorList al = AuthorList.getAuthorList(fieldText);

        addBody(sb, al, "author");
        return sb.toString();
        
        
        
        
        


        
    }

    public void addBody(StringBuilder sb, AuthorList al, String tagName) {
        for (int i=0; i<al.size(); i++) {
            sb.append("<"+tagName+">");
            AuthorList.Author a = al.getAuthor(i);
            if ((a.getFirst() != null) && (a.getFirst().length() > 0)) {
                sb.append("<firstname>");
                sb.append(xc.format(a.getFirst()));
                sb.append("</firstname>");
            }
            if ((a.getVon() != null) && (a.getVon().length() > 0)) {
                sb.append("<othername>");
                sb.append(xc.format(a.getVon()));
                sb.append("</othername>");
            }
            if ((a.getLast() != null) && (a.getLast().length() > 0)) {
                sb.append("<lastname>");
                sb.append(xc.format(a.getLast()));
                if ((a.getJr() != null) && (a.getJr().length() > 0)) {
                    sb.append(" "+xc.format(a.getJr()));
                }
                sb.append("</lastname>");
            }

            if (i < al.size()-1)
                sb.append("</"+tagName+">\n       ");
            else
                sb.append("</"+tagName+">");
        }
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



