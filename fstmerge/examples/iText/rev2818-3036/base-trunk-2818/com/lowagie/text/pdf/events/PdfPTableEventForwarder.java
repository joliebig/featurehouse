

package com.lowagie.text.pdf.events;

import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPTable;
import com.lowagie.text.pdf.PdfPTableEvent;



public class PdfPTableEventForwarder implements PdfPTableEvent {

    
    protected ArrayList events = new ArrayList();
    
    
    public void addTableEvent(PdfPTableEvent event) {
        events.add(event);
    }

    
    public void tableLayout(PdfPTable table, float[][] widths, float[] heights, int headerRows, int rowStart, PdfContentByte[] canvases) {
        PdfPTableEvent event;
        for (Iterator i = events.iterator(); i.hasNext(); ) {
            event = (PdfPTableEvent)i.next();
            event.tableLayout(table, widths, heights, headerRows, rowStart, canvases);
        }
    }
}