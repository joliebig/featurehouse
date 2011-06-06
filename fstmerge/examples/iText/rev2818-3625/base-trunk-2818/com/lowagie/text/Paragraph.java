

package com.lowagie.text;



public class Paragraph extends Phrase {
    
    
    private static final long serialVersionUID = 7852314969733375514L;
    
    
    
    
    protected int alignment = Element.ALIGN_UNDEFINED;
    
    
    protected float multipliedLeading = 0;
    
    
    protected float indentationLeft;
    
    
    protected float indentationRight;
    
    
    private float firstLineIndent = 0;
    
    
    protected float spacingBefore;
    
    
    protected float spacingAfter;
    
    
    private float extraParagraphSpace = 0;
    
    
    protected boolean keeptogether = false;
    
    
    
    
    public Paragraph() {
        super();
    }
    
    
    public Paragraph(float leading) {
        super(leading);
    }
    
        
    public Paragraph(Chunk chunk) {
        super(chunk);
    }
    
        
    public Paragraph(float leading, Chunk chunk) {
        super(leading, chunk);
    }
    
    
    public Paragraph(String string) {
        super(string);
    }
    
    
    public Paragraph(String string, Font font) {
        super(string, font);
    }
    
    
    public Paragraph(float leading, String string) {
        super(leading, string);
    }
    
    
    public Paragraph(float leading, String string, Font font) {
        super(leading, string, font);
    }
    
        
    public Paragraph(Phrase phrase) {
        super(phrase);
        if (phrase instanceof Paragraph) {
            Paragraph p = (Paragraph)phrase;
            setAlignment(p.alignment);
            setLeading(phrase.getLeading(), p.multipliedLeading);
            setIndentationLeft(p.getIndentationLeft());
            setIndentationRight(p.getIndentationRight());
            setFirstLineIndent(p.getFirstLineIndent());
            setSpacingAfter(p.spacingAfter());
            setSpacingBefore(p.spacingBefore());
            setExtraParagraphSpace(p.getExtraParagraphSpace());
        }
    }
    
    
    
    
    public int type() {
        return Element.PARAGRAPH;
    }
    
    
    
    
    public boolean add(Object o) {
        if (o instanceof List) {
            List list = (List) o;
            list.setIndentationLeft(list.getIndentationLeft() + indentationLeft);
            list.setIndentationRight(indentationRight);
            return super.add(list);
        }
        else if (o instanceof Image) {
            super.addSpecial(o);
            return true;
        }
        else if (o instanceof Paragraph) {
            super.add(o);
            super.add(Chunk.NEWLINE);
            return true;
        }
        return super.add(o);
    }
    
    
    
    
    public void setAlignment(int alignment) {
        this.alignment = alignment;
    }
    
    
    public void setAlignment(String alignment) {
        if (ElementTags.ALIGN_CENTER.equalsIgnoreCase(alignment)) {
            this.alignment = Element.ALIGN_CENTER;
            return;
        }
        if (ElementTags.ALIGN_RIGHT.equalsIgnoreCase(alignment)) {
            this.alignment = Element.ALIGN_RIGHT;
            return;
        }
        if (ElementTags.ALIGN_JUSTIFIED.equalsIgnoreCase(alignment)) {
            this.alignment = Element.ALIGN_JUSTIFIED;
            return;
        }
        if (ElementTags.ALIGN_JUSTIFIED_ALL.equalsIgnoreCase(alignment)) {
            this.alignment = Element.ALIGN_JUSTIFIED_ALL;
            return;
        }
        this.alignment = Element.ALIGN_LEFT;
    }
    
    
    public void setLeading(float fixedLeading) {
        this.leading = fixedLeading;
        this.multipliedLeading = 0;
    }
    
    
    public void setMultipliedLeading(float multipliedLeading) {
        this.leading = 0;
        this.multipliedLeading = multipliedLeading;
    }
    
    
    public void setLeading(float fixedLeading, float multipliedLeading) {
        this.leading = fixedLeading;
        this.multipliedLeading = multipliedLeading;
    }
    
    
    public void setIndentationLeft(float indentation) {
        this.indentationLeft = indentation;
    }
    
    
    public void setIndentationRight(float indentation) {
        this.indentationRight = indentation;
    }
    
    
    public void setFirstLineIndent(float firstLineIndent) {
        this.firstLineIndent = firstLineIndent;
    }
    
    
    public void setSpacingBefore(float spacing) {
        this.spacingBefore = spacing;
    }
    
    
    public void setSpacingAfter(float spacing) {
        this.spacingAfter = spacing;
    }
    
    
    public void setKeepTogether(boolean keeptogether) {
        this.keeptogether = keeptogether;
    }
    
    
    public boolean getKeepTogether() {
        return keeptogether;
    }

    

    
    public int getAlignment() {
        return alignment;
    }
    
    
    public float getMultipliedLeading() {
        return multipliedLeading;
    }
    
    
    public float getTotalLeading() {
        float m = font == null ?
                Font.DEFAULTSIZE * multipliedLeading : font.getCalculatedLeading(multipliedLeading);
        if (m > 0 && !hasLeading()) {
            return m;
        }
        return getLeading() + m;
    }

    
    public float getIndentationLeft() {
        return indentationLeft;
    }

    
    public float getIndentationRight() {
        return indentationRight;
    }
    
    
    public float getFirstLineIndent() {
        return this.firstLineIndent;
    }
    
    
    public float spacingBefore() {
        return spacingBefore;
    }

    
    public float spacingAfter() {
        return spacingAfter;
    }
    
    
    public float getExtraParagraphSpace() {
        return this.extraParagraphSpace;
    }
    
    
    public void setExtraParagraphSpace(float extraParagraphSpace) {
        this.extraParagraphSpace = extraParagraphSpace;
    }

    
    
    
    public Paragraph(java.util.Properties attributes) {
        this(com.lowagie.text.factories.ElementFactory.getParagraph(attributes));
    }
    
    
    public int alignment() {
        return getAlignment();
    }
    
    
    public float indentationLeft() {
        return getIndentationLeft();
    }
    
    
    public float indentationRight() {
        return getIndentationRight();
    }
}
