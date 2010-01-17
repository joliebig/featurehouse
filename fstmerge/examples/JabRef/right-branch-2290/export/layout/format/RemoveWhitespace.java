





















package net.sf.jabref.export.layout.format;

import net.sf.jabref.export.layout.LayoutFormatter;



public class RemoveWhitespace implements LayoutFormatter
{
    

    public String format(String fieldText)
    {
        String fieldEntry = fieldText;
        StringBuffer sb = new StringBuffer(fieldEntry.length());

        for (int i = 0; i < fieldEntry.length(); i++)
        {
            
            if ( !Character.isWhitespace(fieldEntry.charAt(i)) || Character.isSpaceChar(fieldEntry.charAt(i)))
            {
                
                sb.append(fieldEntry.charAt(i));
            }
        }

        fieldEntry = sb.toString();
        return fieldEntry;
    }
}



