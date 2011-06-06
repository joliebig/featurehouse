

package com.lowagie.text.pdf.events;

import java.util.ArrayList;

import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPTable;
import com.lowagie.text.pdf.PdfPTableEvent;



public class PdfPTableEventForwarder implements PdfPTableEvent {

    
    protected ArrayList<PdfPTableEvent> events = new ArrayList<PdfPTableEvent>();
    
    
    public void addTableEvent(PdfPTableEvent event) {
        events.add(event);
    }

    
    public void tableLayout(PdfPTable table, float[][] widths, float[] heights, int headerRows, int rowStart, PdfContentByte[] canvases) {
        for (PdfPTableEvent event: events) {
            event.tableLayout(table, widths, heights, headerRows, rowStart, canvases);
        }
    }
}