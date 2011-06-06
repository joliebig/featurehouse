

package com.lowagie.text.pdf;


public interface PdfPTableEvent {
    
        
    public void tableLayout(PdfPTable table, float widths[][], float heights[], int headerRows, int rowStart, PdfContentByte[] canvases);

}

