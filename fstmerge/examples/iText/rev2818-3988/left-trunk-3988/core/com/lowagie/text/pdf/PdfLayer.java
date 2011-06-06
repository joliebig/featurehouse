

package com.lowagie.text.pdf;

import java.util.ArrayList;

public class PdfLayer extends PdfDictionary implements PdfOCG {
    protected PdfIndirectReference ref;
    protected ArrayList children;
    protected PdfLayer parent;
    protected String title;

    
    private boolean on = true;
    
    
    private boolean onPanel = true;
    
    PdfLayer(String title) {
        this.title = title;
    }
    
        
    public static PdfLayer createTitle(String title, PdfWriter writer) {
        if (title == null)
            throw new NullPointerException("Title cannot be null.");
        PdfLayer layer = new PdfLayer(title);
        writer.registerLayer(layer);
        return layer;
    }
        
    public PdfLayer(String name, PdfWriter writer) {
        super(PdfName.OCG);
        setName(name);
        ref = writer.getPdfIndirectReference();
        writer.registerLayer(this);
    }
    
    String getTitle() {
        return title;
    }
    
        
    public void addChild(PdfLayer child) {
        if (child.parent != null)
            throw new IllegalArgumentException("The layer '" + ((PdfString)child.get(PdfName.NAME)).toUnicodeString() + "' already has a parent.");
        child.parent = this;
        if (children == null)
            children = new ArrayList();
        children.add(child);
    }

    
        
    public PdfLayer getParent() {
        return parent;
    }
    
        
    public ArrayList getChildren() {
        return children;
    }
    
        
    public PdfIndirectReference getRef() {
        return ref;
    }
    
    
    void setRef(PdfIndirectReference ref) {
        this.ref = ref;
    }
    
        
    public void setName(String name) {
        put(PdfName.NAME, new PdfString(name, PdfObject.TEXT_UNICODE));
    }
    
        
    public PdfObject getPdfObject() {
        return this;
    }
    
    
    public boolean isOn() {
        return this.on;
    }
    
    
    public void setOn(boolean on) {
        this.on = on;
    }
    
    private PdfDictionary getUsage() {
        PdfDictionary usage = (PdfDictionary)get(PdfName.USAGE);
        if (usage == null) {
            usage = new PdfDictionary();
            put(PdfName.USAGE, usage);
        }
        return usage;
    }
    
        
    public void setCreatorInfo(String creator, String subtype) {
        PdfDictionary usage = getUsage();
        PdfDictionary dic = new PdfDictionary();
        dic.put(PdfName.CREATOR, new PdfString(creator, PdfObject.TEXT_UNICODE));
        dic.put(PdfName.SUBTYPE, new PdfName(subtype));
        usage.put(PdfName.CREATORINFO, dic);
    }
    
        
    public void setLanguage(String lang, boolean preferred) {
        PdfDictionary usage = getUsage();
        PdfDictionary dic = new PdfDictionary();
        dic.put(PdfName.LANG, new PdfString(lang, PdfObject.TEXT_UNICODE));
        if (preferred)
            dic.put(PdfName.PREFERRED, PdfName.ON);
        usage.put(PdfName.LANGUAGE, dic);
    }
    
        
    public void setExport(boolean export) {
        PdfDictionary usage = getUsage();
        PdfDictionary dic = new PdfDictionary();
        dic.put(PdfName.EXPORTSTATE, export ? PdfName.ON : PdfName.OFF);
        usage.put(PdfName.EXPORT, dic);
    }
    
        
    public void setZoom(float min, float max) {
        if (min <= 0 && max < 0)
            return;
        PdfDictionary usage = getUsage();
        PdfDictionary dic = new PdfDictionary();
        if (min > 0)
            dic.put(PdfName.MIN_LOWER_CASE, new PdfNumber(min));
        if (max >= 0)
            dic.put(PdfName.MAX_LOWER_CASE, new PdfNumber(max));
        usage.put(PdfName.ZOOM, dic);
    }

        
    public void setPrint(String subtype, boolean printstate) {
        PdfDictionary usage = getUsage();
        PdfDictionary dic = new PdfDictionary();
        dic.put(PdfName.SUBTYPE, new PdfName(subtype));
        dic.put(PdfName.PRINTSTATE, printstate ? PdfName.ON : PdfName.OFF);
        usage.put(PdfName.PRINT, dic);
    }

        
    public void setView(boolean view) {
        PdfDictionary usage = getUsage();
        PdfDictionary dic = new PdfDictionary();
        dic.put(PdfName.VIEWSTATE, view ? PdfName.ON : PdfName.OFF);
        usage.put(PdfName.VIEW, dic);
    }
    
    
    public boolean isOnPanel() {
        return this.onPanel;
    }
    
    
    public void setOnPanel(boolean onPanel) {
        this.onPanel = onPanel;
    }
    
}
