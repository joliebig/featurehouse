





















package net.sf.jabref.export.layout.format;

import net.sf.jabref.export.layout.LayoutFormatter;



public class RemoveBrackets implements LayoutFormatter
{
    

    public String format(String fieldText)
    {
        String fieldEntry = fieldText;
        StringBuffer sb = new StringBuffer(fieldEntry.length());

        for (int i = 0; i < fieldEntry.length(); i++)
        {
            
            if ((fieldEntry.charAt(i) != '{') && (fieldEntry.charAt(i) != '}'))
            {
                
                sb.append(fieldEntry.charAt(i));
            }
        }

        fieldEntry = sb.toString();
        return fieldEntry;
    }
}



