
package com.lowagie.text;

import com.lowagie.text.factories.GreekAlphabetFactory;



public class GreekList extends List {


    
    
    public GreekList() {
        super(true);
        setGreekFont();
    }
    
    public GreekList(int symbolIndent) {
        super(true, symbolIndent);
        setGreekFont();
    }

    
    public GreekList(boolean greeklower, int symbolIndent) {
        super(true, symbolIndent);
        lowercase = greeklower;
        setGreekFont();
    }


    
    
    protected void setGreekFont() {
        float fontsize = symbol.getFont().getSize();
        symbol.setFont(FontFactory.getFont(FontFactory.SYMBOL, fontsize, Font.NORMAL));
    }


    
    
    public boolean add(Object o) {
        if (o instanceof ListItem) {
            ListItem item = (ListItem) o;
            Chunk chunk = new Chunk(GreekAlphabetFactory.getString(first + list.size(), lowercase), symbol.getFont());
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

}
