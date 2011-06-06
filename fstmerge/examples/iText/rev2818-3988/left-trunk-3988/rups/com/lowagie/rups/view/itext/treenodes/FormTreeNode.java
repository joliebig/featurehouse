

package com.lowagie.rups.view.itext.treenodes;

import com.lowagie.rups.view.icons.IconTreeNode;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;


public class FormTreeNode extends IconTreeNode {
    
    
    protected PdfObjectTreeNode object_node;
    
    
    public FormTreeNode() {
        super("form.png", "Form");
    }
    
    
    public FormTreeNode(PdfObjectTreeNode node) {
        super("form.png");
        this.object_node = node;
        if (node.isDictionary()) {
            PdfDictionary dict = (PdfDictionary)node.getPdfObject();
            PdfObject fieldname = dict.get(PdfName.T);
            if (fieldname != null) {
                this.setUserObject(fieldname);
            }
            else {
                this.setUserObject("unnamed field");
            }
        }
    }

    
    public PdfObjectTreeNode getCorrespondingPdfObjectNode() {
        return object_node;
    }

    
    private static final long serialVersionUID = 7800080437550790989L;
}