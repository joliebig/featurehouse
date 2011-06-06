
package com.lowagie.text;



public class ZapfDingbatsList extends List {

    
    protected int zn;

    
    public ZapfDingbatsList(int zn) {
        super(true);
        this.zn = zn;
        float fontsize = symbol.getFont().getSize();
        symbol.setFont(FontFactory.getFont(FontFactory.ZAPFDINGBATS, fontsize, Font.NORMAL));
    }

    
    public ZapfDingbatsList(int zn, int symbolIndent) {
        super(true, symbolIndent);
        this.zn = zn;
        float fontsize = symbol.getFont().getSize();
        symbol.setFont(FontFactory.getFont(FontFactory.ZAPFDINGBATS, fontsize, Font.NORMAL));
    }

    
    public void setCharNumber(int zn) {
        this.zn = zn;
    }

    
    public int getCharNumber() {
        return zn;
    }

    
    public boolean add(Object o) {
        if (o instanceof ListItem) {
            ListItem item = (ListItem) o;
            Chunk chunk = new Chunk((char)zn, symbol.getFont());
            chunk.append(" ");
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
