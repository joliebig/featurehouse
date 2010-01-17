
package net.sf.jabref.export.layout;

import wsi.ra.types.StringInt;

import java.util.Vector;

import net.sf.jabref.BibtexDatabase;
import net.sf.jabref.BibtexEntry;
import java.io.IOException;



public class Layout
{
    

    private LayoutEntry[] layoutEntries;

    

    public Layout(Vector parsedEntries, String classPrefix)  throws Exception
    {
        StringInt si;
        Vector tmpEntries = new Vector(parsedEntries.size());

        
        Vector blockEntries = null;
        LayoutEntry le;
        String blockStart = null;

        for (int i = 0; i < parsedEntries.size(); i++)
        {
            si = (StringInt) parsedEntries.get(i);

            
            if (si.i == LayoutHelper.IS_LAYOUT_TEXT)
            {
            }
            else if (si.i == LayoutHelper.IS_SIMPLE_FIELD)
            {
            }
            else if (si.i == LayoutHelper.IS_FIELD_START)
            {
                blockEntries = new Vector();
                blockStart = si.s;
            }
            else if (si.i == LayoutHelper.IS_FIELD_END)
            {
                if (blockStart.equals(si.s))
                {
                    blockEntries.add(si);
                    le = new LayoutEntry(blockEntries, classPrefix, LayoutHelper.IS_FIELD_START);
                    tmpEntries.add(le);
                    blockEntries = null;
                }
                else
                {
                    System.out.println(blockStart+"\n"+si.s);
                    System.out.println(
                        "Nested field entries are not implemented !!!");
                    
                    Thread.dumpStack();
                }
            }
            else if (si.i == LayoutHelper.IS_GROUP_START)
            {
                blockEntries = new Vector();
                blockStart = si.s;
            }
            else if (si.i == LayoutHelper.IS_GROUP_END)
            {
                if (blockStart.equals(si.s))
                {
                    blockEntries.add(si);
                    le = new LayoutEntry(blockEntries, classPrefix, LayoutHelper.IS_GROUP_START);
                    tmpEntries.add(le);
                    blockEntries = null;
                } else
                {
                    System.out.println(
                        "Nested field entries are not implemented !!!");
                    Thread.dumpStack();

                }                
            }
            else if (si.i == LayoutHelper.IS_OPTION_FIELD)
            {
            }
            
            if (blockEntries == null)
            {
                tmpEntries.add(new LayoutEntry(si, classPrefix));
            }
            else
            {
                blockEntries.add(si);
            }
        }

        layoutEntries = new LayoutEntry[tmpEntries.size()];

        for (int i = 0; i < tmpEntries.size(); i++)
        {
            layoutEntries[i] = (LayoutEntry) tmpEntries.get(i);

            
        }
    }

    
    public String doLayout(BibtexEntry bibtex, BibtexDatabase database)
    {
        StringBuffer sb = new StringBuffer(100);

        for (int i = 0; i < layoutEntries.length; i++)
        {
            String fieldText = layoutEntries[i].doLayout(bibtex, database);

            
            
            
            
            if (fieldText == null)
                fieldText = "";
            
            sb.append(fieldText);
        }

        return sb.toString();
    }
    
    
    public String doLayout(BibtexDatabase database, String encoding)
    {
        
        StringBuffer sb = new StringBuffer(100);
        String fieldText;
        boolean previousSkipped = false;

        for (int i = 0; i < layoutEntries.length; i++)
        {
            fieldText = layoutEntries[i].doLayout(database, encoding);

            if (fieldText == null) 
            {
                fieldText = "";
                if (previousSkipped)
                {
                    int eol = 0;

                    while ((eol < fieldText.length()) &&
                            ((fieldText.charAt(eol) == '\n') ||
                            (fieldText.charAt(eol) == '\r')))
                    {
                        eol++;
                    }

                    if (eol < fieldText.length())
                    {
                        sb.append(fieldText.substring(eol));
                    }
                }
            }
            else
            {
                sb.append(fieldText);
            }

            previousSkipped = false;
        }

        return sb.toString();
    }
    
}



