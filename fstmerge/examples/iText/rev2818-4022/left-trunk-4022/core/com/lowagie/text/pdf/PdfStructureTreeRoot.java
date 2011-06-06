
package com.lowagie.text.pdf;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;


public class PdfStructureTreeRoot extends PdfDictionary {
    
    private HashMap parentTree = new HashMap();
    private PdfIndirectReference reference;

    
    private PdfWriter writer;
    
    
    PdfStructureTreeRoot(PdfWriter writer) {
        super(PdfName.STRUCTTREEROOT);
        this.writer = writer;
        reference = writer.getPdfIndirectReference();
    }
    
        
    public void mapRole(PdfName used, PdfName standard) {
        PdfDictionary rm = (PdfDictionary)get(PdfName.ROLEMAP);
        if (rm == null) {
            rm = new PdfDictionary();
            put(PdfName.ROLEMAP, rm);
        }
        rm.put(used, standard);
    }
    
    
    public PdfWriter getWriter() {
        return this.writer;
    }

        
    public PdfIndirectReference getReference() {
        return this.reference;
    }
    
    void setPageMark(int page, PdfIndirectReference struc) {
        Integer i = new Integer(page);
        PdfArray ar = (PdfArray)parentTree.get(i);
        if (ar == null) {
            ar = new PdfArray();
            parentTree.put(i, ar);
        }
        ar.add(struc);
    }
    
    private void nodeProcess(PdfDictionary struc, PdfIndirectReference reference) throws IOException {
        PdfObject obj = struc.get(PdfName.K);
        if (obj != null && obj.isArray() && !((PdfObject)((PdfArray)obj).getArrayList().get(0)).isNumber()) {
            PdfArray ar = (PdfArray)obj;
            ArrayList a = ar.getArrayList();
            for (int k = 0; k < a.size(); ++k) {
                PdfStructureElement e = (PdfStructureElement)a.get(k);
                a.set(k, e.getReference());
                nodeProcess(e, e.getReference());
            }
        }
        if (reference != null)
            writer.addToBody(struc, reference);
    }
    
    void buildTree() throws IOException {
        HashMap numTree = new HashMap();
        for (Iterator it = parentTree.keySet().iterator(); it.hasNext();) {
            Integer i = (Integer)it.next();
            PdfArray ar = (PdfArray)parentTree.get(i);
            numTree.put(i, writer.addToBody(ar).getIndirectReference());
        }
        PdfDictionary dicTree = PdfNumberTree.writeTree(numTree, writer);
        if (dicTree != null)
            put(PdfName.PARENTTREE, writer.addToBody(dicTree).getIndirectReference());
        
        nodeProcess(this, reference);
    }
}
