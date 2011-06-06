

package com.lowagie.text.pdf;


class ColorDetails {

    
    PdfIndirectReference indirectReference;
    
    PdfName colorName;
    
    PdfSpotColor spotcolor;

    
    ColorDetails(PdfName colorName, PdfIndirectReference indirectReference, PdfSpotColor scolor) {
        this.colorName = colorName;
        this.indirectReference = indirectReference;
        this.spotcolor = scolor;
    }

    
    PdfIndirectReference getIndirectReference() {
        return indirectReference;
    }

    
    PdfName getColorName() {
        return colorName;
    }

    
    PdfObject getSpotColor(PdfWriter writer) {
        return spotcolor.getSpotObject(writer);
    }
}
