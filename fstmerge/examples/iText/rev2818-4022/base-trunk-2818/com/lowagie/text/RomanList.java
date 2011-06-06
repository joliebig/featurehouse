
package com.lowagie.text;

import com.lowagie.text.factories.RomanNumberFactory;



public class RomanList extends List {


    
    
    public RomanList() {
        super(true);
    }

    
    public RomanList(int symbolIndent) {
        super(true, symbolIndent);
    }

    
    public RomanList(boolean lowercase, int symbolIndent) {
        super(true, symbolIndent);
        this.lowercase = lowercase;
    }


    
    
    public boolean add(Object o) {
        if (o instanceof ListItem) {
            ListItem item = (ListItem) o;
            Chunk chunk;
            chunk = new Chunk(RomanNumberFactory.getString(first + list.size(), lowercase), symbol.getFont());
            chunk.append(". ");
            item.setListSymbol(chunk);
            item.setIndentationLeft(symbolIndent, autoindent);
            item.setIndentationRight(0);
            list.add(item);
        } else if (o instanceof List) {
            List nested = (List) o;
            nested.setIndentationLeft(nested.getIndentationLeft() + symbolIndent);
            first--;
            return list.add(nested);
        } else if (o instanceof String) {
            return this.add(new ListItem((String) o));
        }
        return false;
    }
    

    
    
    public static String toRoman(int number) {
        return RomanNumberFactory.getString(number);
    }
    
    public static String toRomanLowerCase(int number) {
        return RomanNumberFactory.getString(number, true);
    }
    
    public static String toRomanUpperCase(int number) {
        return RomanNumberFactory.getString(number, false);
    }

    
    public void setRomanLower(boolean romanlower) {
        setLowercase(romanlower);
    }

    
    public boolean isRomanLower() {
        return isLowercase();
    }
}
