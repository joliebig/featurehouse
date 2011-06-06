

package com.lowagie.text;




public class HeaderFooter extends Rectangle {
    
    
    

    private boolean numbered;
    

    private Phrase before = null;
    

    private int pageN;
    

    private Phrase after = null;
    

    private int alignment;
    
    
    

    
    public HeaderFooter(Phrase before, Phrase after) {
        super(0, 0, 0, 0);
        setBorder(TOP + BOTTOM);
        setBorderWidth(1);
        
        numbered = true;
        this.before = before;
        this.after = after;
    }
    

    
    public HeaderFooter(Phrase before, boolean numbered) {
        super(0, 0, 0, 0);
        setBorder(TOP + BOTTOM);
        setBorderWidth(1);
        
        this.numbered = numbered;
        this.before = before;
    }
    
    
    

    
    public boolean isNumbered() {
        return numbered;
    }
    

    
    public Phrase getBefore() {
        return before;
    }
    

    
    public Phrase getAfter() {
        return after;
    }
    

    
    public void setPageNumber(int pageN) {
        this.pageN = pageN;
    }
    

    
    public void setAlignment(int alignment) {
        this.alignment = alignment;
    }

    
    

    
    public Paragraph paragraph() {
        Paragraph paragraph = new Paragraph(before.getLeading());
        paragraph.add(before);
        if (numbered) {
            paragraph.addSpecial(new Chunk(String.valueOf(pageN), before.getFont()));
        }
        if (after != null) {
            paragraph.addSpecial(after);
        }
        paragraph.setAlignment(alignment);
        return paragraph;
    }

    

        public int alignment() {
            return alignment;
        }

}