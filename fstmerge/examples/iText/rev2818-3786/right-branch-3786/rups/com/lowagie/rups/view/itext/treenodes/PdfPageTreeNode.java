

package com.lowagie.rups.view.itext.treenodes;

import com.lowagie.text.pdf.PdfDictionary;


public class PdfPageTreeNode extends PdfObjectTreeNode {
    
    public PdfPageTreeNode(PdfDictionary object) {
        super("page.png", object);
    }
    
    
    private static final long serialVersionUID = 3747496604295843783L;
}
