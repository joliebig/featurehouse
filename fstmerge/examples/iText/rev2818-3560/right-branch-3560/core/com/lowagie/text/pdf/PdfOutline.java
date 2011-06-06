

package com.lowagie.text.pdf;

import java.awt.Color;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;

import com.lowagie.text.Chunk;
import com.lowagie.text.Font;
import com.lowagie.text.Paragraph;



public class PdfOutline extends PdfDictionary {
    
    
    
    
    private PdfIndirectReference reference;
    
    
    private int count = 0;
    
    
    private PdfOutline parent;
    
    
    private PdfDestination destination;
    
    
    private PdfAction action;
       
    protected ArrayList<PdfOutline> kids = new ArrayList<PdfOutline>();
    
    protected PdfWriter writer;
    
    
    private String tag;
    
    
    private boolean open;
    
    
    private Color color;
    
    
    private int style = 0;
    
    
    
    
    
    PdfOutline(PdfWriter writer) {
        super(OUTLINES);
        open = true;
        parent = null;
        this.writer = writer;
    }
    
    
    
    public PdfOutline(PdfOutline parent, PdfAction action, String title) {
        this(parent, action, title, true);
    }
    
    
    public PdfOutline(PdfOutline parent, PdfAction action, String title, boolean open) {
        super();
        this.action = action;
        initOutline(parent, title, open);
    }
    
    
    
    public PdfOutline(PdfOutline parent, PdfDestination destination, String title) {
        this(parent, destination, title, true);
    }
    
    
    public PdfOutline(PdfOutline parent, PdfDestination destination, String title, boolean open) {
        super();
        this.destination = destination;
        initOutline(parent, title, open);
    }
    
    
    public PdfOutline(PdfOutline parent, PdfAction action, PdfString title) {
        this(parent, action, title, true);
    }
    
    
    public PdfOutline(PdfOutline parent, PdfAction action, PdfString title, boolean open) {
        this(parent, action, title.toString(), open);
    }
    
    
    
    public PdfOutline(PdfOutline parent, PdfDestination destination, PdfString title) {
        this(parent, destination, title, true);
    }
    
    
    public PdfOutline(PdfOutline parent, PdfDestination destination, PdfString title, boolean open) {
        this(parent, destination, title.toString(), true);
    }
    
    
    
    public PdfOutline(PdfOutline parent, PdfAction action, Paragraph title) {
        this(parent, action, title, true);
    }
    
    
    public PdfOutline(PdfOutline parent, PdfAction action, Paragraph title, boolean open) {
        super();
        StringBuffer buf = new StringBuffer();
        for (Chunk chunk: title.getChunks()) {
            buf.append(chunk.getContent());
        }
        this.action = action;
        initOutline(parent, buf.toString(), open);
    }
    
    
    
    public PdfOutline(PdfOutline parent, PdfDestination destination, Paragraph title) {
        this(parent, destination, title, true);
    }
    
    
    public PdfOutline(PdfOutline parent, PdfDestination destination, Paragraph title, boolean open) {
        super();
        StringBuffer buf = new StringBuffer();
        for (Chunk chunk: title.getChunks()) {
            buf.append(chunk.getContent());
        }
        this.destination = destination;
        initOutline(parent, buf.toString(), open);
    }
    
    
    
    
    
    void initOutline(PdfOutline parent, String title, boolean open) {
        this.open = open;
        this.parent = parent;
        writer = parent.writer;
        put(PdfName.TITLE, new PdfString(title, PdfObject.TEXT_UNICODE));
        parent.addKid(this);
        if (destination != null && !destination.hasPage()) 
            setDestinationPage(writer.getCurrentPage());
    }
    
    
    
    public void setIndirectReference(PdfIndirectReference reference) {
        this.reference = reference;
    }
    
    
    
    public PdfIndirectReference indirectReference() {
        return reference;
    }
    
    
    
    public PdfOutline parent() {
        return parent;
    }
    
    
    
    public boolean setDestinationPage(PdfIndirectReference pageReference) {
        if (destination == null) {
            return false;
        }
        return destination.addPage(pageReference);
    }
    
    
    public PdfDestination getPdfDestination() {
        return destination;
    }
    
    int getCount() {
        return count;
    }

    void setCount(int count) {
        this.count = count;
    }
    
    
    
    public int level() {
        if (parent == null) {
            return 0;
        }
        return (parent.level() + 1);
    }
    
    
    
    public void toPdf(PdfWriter writer, OutputStream os) throws IOException {
        if (color != null && !color.equals(Color.black)) {
            put(PdfName.C, new PdfArray(new float[]{color.getRed()/255f,color.getGreen()/255f,color.getBlue()/255f}));
        }
        int flag = 0;
        if ((style & Font.BOLD) != 0)
            flag |= 2;
        if ((style & Font.ITALIC) != 0)
            flag |= 1;
        if (flag != 0)
            put(PdfName.F, new PdfNumber(flag));
        if (parent != null) {
            put(PdfName.PARENT, parent.indirectReference());
        }
        if (destination != null && destination.hasPage()) {
            put(PdfName.DEST, destination);
        }
        if (action != null)
            put(PdfName.A, action);
        if (count != 0) {
            put(PdfName.COUNT, new PdfNumber(count));
        }
        super.toPdf(writer, os);
    }
    
    
    public void addKid(PdfOutline outline) {
        kids.add(outline);
    }
    
    
    public ArrayList<PdfOutline> getKids() {
        return kids;
    }
    
    
    public void setKids(ArrayList<PdfOutline> kids) {
        this.kids = kids;
    }
    
    
    public String getTag() {
        return tag;
    }
    
    
    public void setTag(String tag) {
        this.tag = tag;
    }
    
    
    public String getTitle() {
        PdfString title = (PdfString)get(PdfName.TITLE);
        return title.toString();
    }
    
    
    public void setTitle(String title) {
        put(PdfName.TITLE, new PdfString(title, PdfObject.TEXT_UNICODE));
    }
    
    
    public boolean isOpen() {
        return open;
    }
    
    
    public void setOpen(boolean open) {
        this.open = open;
    }
    
    
    public Color getColor() {
        return this.color;
    }
    
    
    public void setColor(Color color) {
        this.color = color;
    }
    
    
    public int getStyle() {
        return this.style;
    }
    
    
    public void setStyle(int style) {
        this.style = style;
    }
    
}