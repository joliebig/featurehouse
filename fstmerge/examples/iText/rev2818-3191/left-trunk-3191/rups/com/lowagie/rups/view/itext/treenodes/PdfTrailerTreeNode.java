

package com.lowagie.rups.view.itext.treenodes;

import com.lowagie.text.pdf.PdfDictionary;


public class PdfTrailerTreeNode extends PdfObjectTreeNode {

    
    public PdfTrailerTreeNode() {
        super("pdf.png", null);
        setUserObject("Open a PDF file");
    }

    
    public void setTrailer(PdfDictionary trailer) {
        object = trailer;
    }

    
    private static final long serialVersionUID = -3607980103983635182L;

}
