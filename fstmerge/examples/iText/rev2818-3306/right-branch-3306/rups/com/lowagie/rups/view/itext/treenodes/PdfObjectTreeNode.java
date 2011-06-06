

package com.lowagie.rups.view.itext.treenodes;

import com.lowagie.rups.view.icons.IconFetcher;
import com.lowagie.rups.view.icons.IconTreeNode;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfIndirectReference;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfObject;
import com.lowagie.text.pdf.PdfString;


public class PdfObjectTreeNode extends IconTreeNode {

    
    protected PdfObject object;
    
    protected PdfName key = null;
    
    protected int number = -1;
    
    protected boolean recursive = false;

    
    protected PdfObjectTreeNode(PdfObject object) {
        super(null, getCaption(object));
        this.object = object;
        switch(object.type()) {
        case PdfObject.INDIRECT:
            if (isRecursive())
                icon = IconFetcher.getIcon("ref_recursive.png");
            else
                icon = IconFetcher.getIcon("ref.png");
            return;
        case PdfObject.ARRAY:
            icon = IconFetcher.getIcon("array.png");
            return;
        case PdfObject.DICTIONARY:
            icon = IconFetcher.getIcon("dictionary.png");
            return;
        case PdfObject.STREAM:
            icon = IconFetcher.getIcon("stream.png");
            return;
        case PdfObject.BOOLEAN:
            icon = IconFetcher.getIcon("boolean.png");
            return;
        case PdfObject.NAME:
            icon = IconFetcher.getIcon("name.png");
            return;
        case PdfObject.NULL:
            icon = IconFetcher.getIcon("null.png");
            return;
        case PdfObject.NUMBER:
            icon = IconFetcher.getIcon("number.png");
            return;
        case PdfObject.STRING:
            icon = IconFetcher.getIcon("string.png");
            return;
        }
    }
    
    
    protected PdfObjectTreeNode(String icon, PdfObject object) {
        super(icon, getCaption(object));
        this.object = object;
    }
    

    
    public static PdfObjectTreeNode getInstance(PdfObject object) {
        if (object.isDictionary()) {
            if (PdfName.PAGE.equals(((PdfDictionary)object).get(PdfName.TYPE))) {
                return new PdfPageTreeNode((PdfDictionary)object);
            }
            else if (PdfName.PAGES.equals(((PdfDictionary)object).get(PdfName.TYPE))) {
                return new PdfPagesTreeNode((PdfDictionary)object);
            }
        }
        return new PdfObjectTreeNode(object);
    }
    
    
    public static PdfObjectTreeNode getInstance(PdfObject object, int number) {
        PdfObjectTreeNode node = getInstance(object);
        node.number = number;
        return node;
    }

    
    public static PdfObjectTreeNode getInstance(PdfDictionary dict, PdfName key) {
        PdfObjectTreeNode node = getInstance(dict.get(key));
        node.setUserObject(getDictionaryEntryCaption(dict, key));
        node.key = key;
        return node;
    }
    
    
    public PdfObject getPdfObject() {
        return object;
    }

    
    public int getNumber() {
        if (isIndirectReference()) {
            return ((PdfIndirectReference)object).getNumber();
        }
        return number;
    }
    
    
    public boolean isIndirectReference() {
        return object.type() == PdfObject.INDIRECT;
    }
    
    
    public boolean isIndirect() {
        return isIndirectReference() || number > -1;
    }
    
    
    public boolean isArray() {
        return object.isArray();
    }
    
    
    public boolean isDictionaryNode(PdfName key) {
        if (key == null) return false;
        return key.equals(this.key);
    }
    
    
    public boolean isDictionary() {
        return object.isDictionary();
    }
    
    
    public boolean isStream() {
        return object.isStream();
    }

    
    public void setRecursive(boolean recursive) {
        this.recursive = recursive;
    }
    
    
    public boolean isRecursive() {
        return recursive;
    }
    
    
    public static String getCaption(PdfObject object) {
        if (object == null)
            return "null";
        switch (object.type()) {
        case PdfObject.INDIRECT:
            return "Indirect reference: " + object.toString();
        case PdfObject.ARRAY:
            return "Array";
        case PdfObject.STREAM:
            return "Stream";
        case PdfObject.STRING:
            return ((PdfString)object).toUnicodeString();
        }
        return object.toString();
    }

    
    public static String getDictionaryEntryCaption(PdfDictionary dict, PdfName key) {
        StringBuffer buf = new StringBuffer(key.toString());
        buf.append(": ");
        buf.append(dict.get(key).toString());
        return buf.toString();
    }

    
    public PdfObjectTreeNode getAncestor() {
        if (isRecursive()) {
            PdfObjectTreeNode node = this;
            while(true) {
                node = (PdfObjectTreeNode)node.getParent();
                if (node.isIndirectReference() && node.getNumber() == getNumber()) {
                    return node;
                }
            }
        }
        return null;
    }

    
    private static final long serialVersionUID = -5617844659397445879L;
}