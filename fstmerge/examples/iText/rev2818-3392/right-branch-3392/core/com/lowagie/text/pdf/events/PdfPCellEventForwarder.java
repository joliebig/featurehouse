

package com.lowagie.text.pdf.events;

import java.util.ArrayList;

import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPCellEvent;



public class PdfPCellEventForwarder implements PdfPCellEvent {

    
    protected ArrayList<PdfPCellEvent> events = new ArrayList<PdfPCellEvent>();
    
    
    public void addCellEvent(PdfPCellEvent event) {
        events.add(event);
    }

    
    public void cellLayout(PdfPCell cell, Rectangle position, PdfContentByte[] canvases) {
        ;
        for (PdfPCellEvent event: events) {
            event.cellLayout(cell, position, canvases);
        }
    }
}