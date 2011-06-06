

package com.lowagie.text;

import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.factories.RomanAlphabetFactory;



public class List implements TextElementArray {
    
    
    
    
    public static final boolean ORDERED = true;
    
    public static final boolean UNORDERED = false;
    
    public static final boolean NUMERICAL = false;
    
    public static final boolean ALPHABETICAL = true;
    
    public static final boolean UPPERCASE = false;
    
    public static final boolean LOWERCASE = true;
    
    
    
    
    protected ArrayList<Element> list = new ArrayList<Element>();
    
    
    protected boolean numbered = false;
    
    protected boolean lettered = false;
    
    protected boolean lowercase = false;
    
    protected boolean autoindent = false;
    
    protected boolean alignindent = false;
    
    
    protected int first = 1;
    
    protected Chunk symbol = new Chunk("- ");
    
    protected String preSymbol = "";
    
    protected String postSymbol = ". ";
    
    
    protected float indentationLeft = 0;
    
    protected float indentationRight = 0;
    
    protected float symbolIndent = 0;
    
    

    
    public List() {
        this(false, false);
    }
    
    
    public List(float symbolIndent) {
        this.symbolIndent = symbolIndent;
    }
    
    
    public List(boolean numbered) {
          this(numbered, false);
    }
        
    
    public List(boolean numbered, boolean lettered) {
        this.numbered = numbered;
        this.lettered = lettered;
        this.autoindent = true;
        this.alignindent = true;
    }
    
    
    public List(boolean numbered, float symbolIndent) {
        this(numbered, false, symbolIndent);
    }
    
    
    public List(boolean numbered, boolean lettered, float symbolIndent) {
        this.numbered = numbered;
        this.lettered = lettered;
        this.symbolIndent = symbolIndent;
    }
    
    
    
    
    public boolean process(ElementListener listener) {
        try {
            for (Iterator<Element> i = list.iterator(); i.hasNext(); ) {
                listener.add(i.next());
            }
            return true;
        }
        catch(DocumentException de) {
            return false;
        }
    }
    
    
    public int type() {
        return Element.LIST;
    }
    
    
    public ArrayList<Chunk> getChunks() {
        ArrayList<Chunk> tmp = new ArrayList<Chunk>();
        for (Iterator<Element> i = list.iterator(); i.hasNext(); ) {
            tmp.addAll(i.next().getChunks());
        }
        return tmp;
    }
    
    
    
    
    public boolean addObject(Object o) {
        if (o instanceof Element) {
            return add((Element) o);
        }
        if (o instanceof String) {
            return this.add(new ListItem((String) o));
        }
        return false;
    }
    
    
    public boolean add(Element o) {
        if (o instanceof ListItem) {
            ListItem item = (ListItem) o;
            if (numbered || lettered) {
                Chunk chunk = new Chunk(preSymbol, symbol.getFont());
                int index = first + list.size();
                if ( lettered )
                    chunk.append(RomanAlphabetFactory.getString(index, lowercase));
                else
                    chunk.append(String.valueOf(index));
                chunk.append(postSymbol);
                item.setListSymbol(chunk);
            }
            else {
                item.setListSymbol(symbol);
            }
            item.setIndentationLeft(symbolIndent, autoindent);
            item.setIndentationRight(0);
            return list.add(item);
        }
        else if (o instanceof List) {
            List nested = (List) o;
            nested.setIndentationLeft(nested.getIndentationLeft() + symbolIndent);
            first--;
            return list.add(nested);
        }
        return false;
    }
    
    
    
    
    public void normalizeIndentation() {
        float max = 0;
        Element o;
        for (Iterator<Element> i = list.iterator(); i.hasNext(); ) {
            o = i.next();
            if (o instanceof ListItem) {
                max = Math.max(max, ((ListItem)o).getIndentationLeft());
            }
        }
        for (Iterator<Element> i = list.iterator(); i.hasNext(); ) {
            o = i.next();
            if (o instanceof ListItem) {
                ((ListItem)o).setIndentationLeft(max);
            }
        }
    }
    
    

    
    public void setNumbered(boolean numbered) {
        this.numbered = numbered;
    }

    
    public void setLettered(boolean lettered) {
        this.lettered = lettered;
    }

    
    public void setLowercase(boolean uppercase) {
        this.lowercase = uppercase;
    }

    
    public void setAutoindent(boolean autoindent) {
        this.autoindent = autoindent;
    }
    
    public void setAlignindent(boolean alignindent) {
        this.alignindent = alignindent;
    }
    
    
    public void setFirst(int first) {
        this.first = first;
    }
    
    
    public void setListSymbol(Chunk symbol) {
        this.symbol = symbol;
    }
    
    
    public void setListSymbol(String symbol) {
        this.symbol = new Chunk(symbol);
    }
    
    
    public void setIndentationLeft(float indentation) {
        this.indentationLeft = indentation;
    }
    
    
    public void setIndentationRight(float indentation) {
        this.indentationRight = indentation;
    }

    
    public void setSymbolIndent(float symbolIndent) {
        this.symbolIndent = symbolIndent;
    }
    
    
    
    
    public ArrayList<Element> getItems() {
        return list;
    }
    
    
    public int size() {
        return list.size();
    }

    
    public boolean isEmpty() {
        return list.isEmpty();
    }

    
    public float getTotalLeading() {
        if (list.size() < 1) {
            return -1;
        }
        ListItem item = (ListItem) list.get(0);
        return item.getTotalLeading();
    }
    
    
    
    
    
    public boolean isNumbered() {
        return numbered;
    }

    
    public boolean isLettered() {
        return lettered;
    }

    
    public boolean isLowercase() {
        return lowercase;
    }
    
    
    public boolean isAutoindent() {
        return autoindent;
    }
    
    
    public boolean isAlignindent() {
        return alignindent;
    }

    
    public int getFirst() {
        return first;
    }

    
    public Chunk getSymbol() {
        return symbol;
    }

    
    public float getIndentationLeft() {
        return indentationLeft;
    }

    
    public float getIndentationRight() {
        return indentationRight;
    }

    
    public float getSymbolIndent() {
        return symbolIndent;
    }
    
    public boolean isContent() {
        return true;
    }

    
    public boolean isNestable() {
        return true;
    }

    
    public String getPostSymbol() {
        return postSymbol;
    }

    
    public void setPostSymbol(String postSymbol) {
        this.postSymbol = postSymbol;
    }

    
    public String getPreSymbol() {
        return preSymbol;
    }

    
    public void setPreSymbol(String preSymbol) {
        this.preSymbol = preSymbol;
    }

}