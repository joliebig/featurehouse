
package com.lowagie.text.pdf;

import com.lowagie.text.Rectangle;


public interface PdfPCellEvent {
        
    public void cellLayout(PdfPCell cell, Rectangle position, PdfContentByte[] canvases);
}
