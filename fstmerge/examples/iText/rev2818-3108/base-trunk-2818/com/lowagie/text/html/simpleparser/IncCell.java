

package com.lowagie.text.html.simpleparser;

import java.util.ArrayList;

import com.lowagie.text.Element;
import com.lowagie.text.ElementListener;
import com.lowagie.text.Phrase;
import com.lowagie.text.TextElementArray;
import com.lowagie.text.html.Markup;
import com.lowagie.text.pdf.PdfPCell;

public class IncCell implements TextElementArray {
    
    private ArrayList chunks = new ArrayList();
    private PdfPCell cell;
    
    
    public IncCell(String tag, ChainedProperties props) {
        cell = new PdfPCell((Phrase)null);
        String value = props.getProperty("colspan");
        if (value != null)
            cell.setColspan(Integer.parseInt(value));
        value = props.getProperty("align");
        if (tag.equals("th"))
            cell.setHorizontalAlignment(Element.ALIGN_CENTER);
        if (value != null) {
            if ("center".equalsIgnoreCase(value))
                cell.setHorizontalAlignment(Element.ALIGN_CENTER);
            else if ("right".equalsIgnoreCase(value))
                cell.setHorizontalAlignment(Element.ALIGN_RIGHT);
            else if ("left".equalsIgnoreCase(value))
                cell.setHorizontalAlignment(Element.ALIGN_LEFT);
            else if ("justify".equalsIgnoreCase(value))
                cell.setHorizontalAlignment(Element.ALIGN_JUSTIFIED);
        }
        value = props.getProperty("valign");
        cell.setVerticalAlignment(Element.ALIGN_MIDDLE);
        if (value != null) {
            if ("top".equalsIgnoreCase(value))
                cell.setVerticalAlignment(Element.ALIGN_TOP);
            else if ("bottom".equalsIgnoreCase(value))
                cell.setVerticalAlignment(Element.ALIGN_BOTTOM);
        }
        value = props.getProperty("border");
        float border = 0;
        if (value != null)
            border = Float.parseFloat(value);
        cell.setBorderWidth(border);
        value = props.getProperty("cellpadding");
        if (value != null)
            cell.setPadding(Float.parseFloat(value));
        cell.setUseDescender(true);
        value = props.getProperty("bgcolor");
        cell.setBackgroundColor(Markup.decodeColor(value));
    }
    
    public boolean add(Object o) {
        if (!(o instanceof Element))
            return false;
        cell.addElement((Element)o);
        return true;
    }
    
    public ArrayList getChunks() {
        return chunks;
    }
    
    public boolean process(ElementListener listener) {
        return true;
    }
    
    public int type() {
        return 0;
    }
    
    public PdfPCell getCell() {
        return cell;
    }    
}