

package com.lowagie.text.pdf;
import java.io.IOException;

import com.lowagie.text.Rectangle;



public class PdfTemplate extends PdfContentByte {
    public static final int TYPE_TEMPLATE = 1;
    public static final int TYPE_IMPORTED = 2;
    public static final int TYPE_PATTERN = 3;
    protected int type;
    
    protected PdfIndirectReference thisReference;
    
    
    protected PageResources pageResources;
    
    
    
    protected Rectangle bBox = new Rectangle(0, 0);
    
    protected PdfArray matrix;
    
    protected PdfTransparencyGroup group;
    
    protected PdfOCG layer;
    
    
    
    protected PdfTemplate() {
        super(null);
        type = TYPE_TEMPLATE;
    }
    
    
    
    PdfTemplate(PdfWriter wr) {
        super(wr);
        type = TYPE_TEMPLATE;
        pageResources = new PageResources();
        pageResources.addDefaultColor(wr.getDefaultColorspace());
        thisReference = writer.getPdfIndirectReference();
    }
    
    
    public static PdfTemplate createTemplate(PdfWriter writer, float width, float height) {
        return createTemplate(writer, width, height, null);
    }
    
    static PdfTemplate createTemplate(PdfWriter writer, float width, float height, PdfName forcedName) {
        PdfTemplate template = new PdfTemplate(writer);
        template.setWidth(width);
        template.setHeight(height);
        writer.addDirectTemplateSimple(template, forcedName);
        return template;
    }

    
    
    public void setWidth(float width) {
        bBox.setLeft(0);
        bBox.setRight(width);
    }
    
    
    
    public void setHeight(float height) {
        bBox.setBottom(0);
        bBox.setTop(height);
    }
    
    
    public float getWidth() {
        return bBox.getWidth();
    }
    
    
    
    public float getHeight() {
        return bBox.getHeight();
    }
    
    public Rectangle getBoundingBox() {
        return bBox;
    }
    
    public void setBoundingBox(Rectangle bBox) {
        this.bBox = bBox;
    }
    
        
    public void setLayer(PdfOCG layer) {
        this.layer = layer;
    }
    
    
    public PdfOCG getLayer() {
        return layer;
    }

    public void setMatrix(float a, float b, float c, float d, float e, float f) {
        matrix = new PdfArray();
        matrix.add(new PdfNumber(a));
        matrix.add(new PdfNumber(b));
        matrix.add(new PdfNumber(c));
        matrix.add(new PdfNumber(d));
        matrix.add(new PdfNumber(e));
        matrix.add(new PdfNumber(f));
    }

    PdfArray getMatrix() {
        return matrix;
    }
    
    
    
    public PdfIndirectReference getIndirectReference() {
        
        if (thisReference == null ) {
            thisReference = writer.getPdfIndirectReference();
        }
        return thisReference;
    }
        
    public void beginVariableText() {
        content.append("/Tx BMC ");
    }
    
    public void endVariableText() {
        content.append("EMC ");
    }
    
    
    
    PdfObject getResources() {
        return getPageResources().getResources();
    }
    
    
    PdfStream getFormXObject(int compressionLevel) throws IOException {
        return new PdfFormXObject(this, compressionLevel);
    }
        
    
    
    public PdfContentByte getDuplicate() {
        PdfTemplate tpl = new PdfTemplate();
        tpl.writer = writer;
        tpl.pdf = pdf;
        tpl.thisReference = thisReference;
        tpl.pageResources = pageResources;
        tpl.bBox = new Rectangle(bBox);
        tpl.group = group;
        tpl.layer = layer;
        if (matrix != null) {
            tpl.matrix = new PdfArray(matrix);
        }
        tpl.separator = separator;
        return tpl;
    }
    
    public int getType() {
        return type;
    }
    
    PageResources getPageResources() {
        return pageResources;
    }
    
    
    public PdfTransparencyGroup getGroup() {
        return this.group;
    }
    
    
    public void setGroup(PdfTransparencyGroup group) {
        this.group = group;
    }
    
}