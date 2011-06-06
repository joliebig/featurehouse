
package com.lowagie.text;



public class ZapfDingbatsNumberList extends List {

    
    protected int type;

    
    public ZapfDingbatsNumberList(int type) {
        super(true);
        this.type = type;
        float fontsize = symbol.getFont().getSize();
        symbol.setFont(FontFactory.getFont(FontFactory.ZAPFDINGBATS, fontsize, Font.NORMAL));
    }

    
    public ZapfDingbatsNumberList(int type, int symbolIndent) {
        super(true, symbolIndent);
        this.type = type;
        float fontsize = symbol.getFont().getSize();
        symbol.setFont(FontFactory.getFont(FontFactory.ZAPFDINGBATS, fontsize, Font.NORMAL));
    }

    
    public void setType(int type) {
        this.type = type;
    }

    
    public int getType() {
        return type;
    }

    
    public boolean add(Object o) {
        if (o instanceof ListItem) {
            ListItem item = (ListItem) o;
            Chunk chunk;
            switch (type ) {
                case 0:
                    chunk = new Chunk((char)(first + list.size() + 171), symbol.getFont());
                    break;
                case 1:
                    chunk = new Chunk((char)(first + list.size() + 181), symbol.getFont());
                    break;
                case 2:
                    chunk = new Chunk((char)(first + list.size() + 191), symbol.getFont());
                    break;
                default:
                    chunk = new Chunk((char)(first + list.size() + 201), symbol.getFont());
            }
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
