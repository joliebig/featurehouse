

package com.lowagie.text;



public class ListItem extends Paragraph {
    
    
    private static final long serialVersionUID = 1970670787169329006L;
    
    
    
    
    private Chunk symbol;
    
    
    
    
    public ListItem() {
        super();
    }
    
        
    public ListItem(float leading) {
        super(leading);
    }
    
    
    public ListItem(Chunk chunk) {
        super(chunk);
    }
    
    
    public ListItem(String string) {
        super(string);
    }
    
    
    public ListItem(String string, Font font) {
        super(string, font);
    }
    
    
    public ListItem(float leading, Chunk chunk) {
        super(leading, chunk);
    }
    
    
    public ListItem(float leading, String string) {
        super(leading, string);
    }
    
    
    public ListItem(float leading, String string, Font font) {
        super(leading, string, font);
    }
    
    
    public ListItem(Phrase phrase) {
        super(phrase);
    }
    
    
    
    
    public int type() {
        return Element.LISTITEM;
    }
    
    
    
    
    public void setListSymbol(Chunk symbol) {
        if (this.symbol == null) {
            this.symbol = symbol;
            if (this.symbol.getFont().isStandardFont()) {
                this.symbol.setFont(font);
            }
        }
    }
    
    
    public void setIndentationLeft(float indentation, boolean autoindent) {
        if (autoindent) {
            setIndentationLeft(getListSymbol().getWidthPoint());
        }
        else {
            setIndentationLeft(indentation);
        }
    }
    
    

    
    public Chunk getListSymbol() {
        return symbol;
    }
        
    
        
    
    public ListItem(java.util.Properties attributes) {
        this(com.lowagie.text.factories.ElementFactory.getParagraph(attributes));
    }
    
    
    public Chunk listSymbol() {
        return getListSymbol();
    }
}
