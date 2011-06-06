

package com.lowagie.rups.view.itext.treenodes;

import com.lowagie.rups.view.icons.IconTreeNode;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;


public class OutlineTreeNode extends IconTreeNode {

    
    protected PdfObjectTreeNode object_node;
    
    
    public OutlineTreeNode() {
        super("outline.png", "Bookmarks");
    }

    
    public OutlineTreeNode(PdfObjectTreeNode node) {
        super("outline.png");
        this.object_node = node;
        PdfDictionary dict = (PdfDictionary)node.getPdfObject();
        this.setUserObject(dict.get(PdfName.TITLE));
    }

    
    public PdfObjectTreeNode getCorrespondingPdfObjectNode() {
        return object_node;
    }

    
    private static final long serialVersionUID = 5437651809665762952L;
}