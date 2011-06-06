

package com.lowagie.rups.view.itext.treenodes;

import com.lowagie.text.pdf.PdfDictionary;


public class PdfPagesTreeNode extends PdfObjectTreeNode {
    
    public PdfPagesTreeNode(PdfDictionary object) {
        super("pages.png", object);
    }

    
    private static final long serialVersionUID = 4527774449030791503L;
}
