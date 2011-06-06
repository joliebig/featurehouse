

package com.lowagie.text.pdf;

import java.util.Collection;
import java.util.HashSet;


public class PdfLayerMembership extends PdfDictionary implements PdfOCG {
    
        
    public static final PdfName ALLON = new PdfName("AllOn");
        
    public static final PdfName ANYON = new PdfName("AnyOn");
        
    public static final PdfName ANYOFF = new PdfName("AnyOff");
        
    public static final PdfName ALLOFF = new PdfName("AllOff");

    PdfIndirectReference ref;
    PdfArray members = new PdfArray();
    HashSet<PdfLayer> layers = new HashSet<PdfLayer>();
    
        
    public PdfLayerMembership(PdfWriter writer) {
        super(PdfName.OCMD);
        put(PdfName.OCGS, members);
        ref = writer.getPdfIndirectReference();
    }
    
        
    public PdfIndirectReference getRef() {
        return ref;
    }
    
        
    public void addMember(PdfLayer layer) {
        if (!layers.contains(layer)) {
            members.add(layer.getRef());
            layers.add(layer);
        }
    }
    
        
    public Collection<PdfLayer> getLayers() {
        return layers;
    }
    
        
    public void setVisibilityPolicy(PdfName type) {
        put(PdfName.P, type);
    }
    
        
    public PdfObject getPdfObject() {
        return this;
    }
}
