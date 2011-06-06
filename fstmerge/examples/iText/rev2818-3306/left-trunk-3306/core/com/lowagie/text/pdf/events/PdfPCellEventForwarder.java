

package com.lowagie.text.pdf.events;

import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPCellEvent;



public class PdfPCellEventForwarder implements PdfPCellEvent {

    
    protected ArrayList events = new ArrayList();
    
    
    public void addCellEvent(PdfPCellEvent event) {
        events.add(event);
    }

    
    public void cellLayout(PdfPCell cell, Rectangle position, PdfContentByte[] canvases) {
        PdfPCellEvent event;
        for (Iterator i = events.iterator(); i.hasNext(); ) {
            event = (PdfPCellEvent)i.next();
            event.cellLayout(cell, position, canvases);
        }
    }
}