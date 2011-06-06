

package com.lowagie.rups.model;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;

import com.lowagie.rups.view.itext.treenodes.PdfObjectTreeNode;
import com.lowagie.rups.view.itext.treenodes.PdfPagesTreeNode;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfIndirectReference;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfNull;
import com.lowagie.text.pdf.PdfObject;


public class TreeNodeFactory {

    
    protected IndirectObjectFactory objects;
    
    protected ArrayList<PdfObjectTreeNode> nodes = new ArrayList<PdfObjectTreeNode>();
    
    
    public TreeNodeFactory(IndirectObjectFactory objects) {
        this.objects = objects;
        for (int i = 0; i < objects.size(); i++) {
            int ref = objects.getRefByIndex(i);
            nodes.add(PdfObjectTreeNode.getInstance(PdfNull.PDFNULL, ref));
        }
    }
    
    
    public PdfObjectTreeNode getNode(int ref) {
        int idx = objects.getIndexByRef(ref);
        PdfObjectTreeNode node = nodes.get(idx);
        if (node.getPdfObject().isNull()) {
            node = PdfObjectTreeNode.getInstance(objects.loadObjectByReference(ref), ref);
            nodes.set(idx, node);
        }
        return node;
    }
    
    
    public void expandNode(PdfObjectTreeNode node) {
        if (node.getChildCount() > 0) {
            return;
        }
        PdfObject object = node.getPdfObject();
        PdfObjectTreeNode leaf;
        switch (object.type()) {
        case PdfObject.INDIRECT:
            PdfIndirectReference ref = (PdfIndirectReference)object;
            leaf = getNode(ref.getNumber());
            addNodes(node, leaf);
            if (leaf instanceof PdfPagesTreeNode)
                expandNode(leaf);
            return;
        case PdfObject.ARRAY:
            PdfArray array = (PdfArray)object;
            for (Iterator it = array.listIterator(); it.hasNext(); ) {
                leaf = PdfObjectTreeNode.getInstance((PdfObject)it.next());
                addNodes(node, leaf);
                expandNode(leaf);
            }
            return;
        case PdfObject.DICTIONARY:
        case PdfObject.STREAM:
            PdfDictionary dict = (PdfDictionary)object;
            for (Iterator it = dict.getKeys().iterator(); it.hasNext(); ) {
                leaf = PdfObjectTreeNode.getInstance(dict, (PdfName)it.next());
                addNodes(node, leaf);
                expandNode(leaf);
            }
            return;
        }
    }
    
    
    public PdfObjectTreeNode getChildNode(PdfObjectTreeNode node, PdfName key) {
        Enumeration children = node.breadthFirstEnumeration();
        PdfObjectTreeNode child;
        while (children.hasMoreElements()) {
            child = (PdfObjectTreeNode)children.nextElement();
            if (child.isDictionaryNode(key)) {
                if (child.isIndirectReference()) {
                    expandNode(child);
                    child = (PdfObjectTreeNode)child.getFirstChild();
                }
                expandNode(child);
                return child;
            }
        }
        return null;
    }
    
    
    private void addNodes(PdfObjectTreeNode parent, PdfObjectTreeNode child) {
        try {
            parent.add(child);
        }
        catch(IllegalArgumentException iae) {
            parent.setRecursive(true);
        }
    }
}
