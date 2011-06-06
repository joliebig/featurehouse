
package com.lowagie.text;



public class ZapfDingbatsNumberList extends List {

    
    protected int type;

    
    public ZapfDingbatsNumberList(int type) {
        super(true);
        this.type = type;
        float fontsize = symbol.getFont().getSize();
        symbol.setFont(FontFactory.getFont(FontFactory.ZAPFDINGBATS, fontsize, Font.NORMAL));
        postSymbol = " ";
    }

    
    public ZapfDingbatsNumberList(int type, int symbolIndent) {
        super(true, symbolIndent);
        this.type = type;
        float fontsize = symbol.getFont().getSize();
        symbol.setFont(FontFactory.getFont(FontFactory.ZAPFDINGBATS, fontsize, Font.NORMAL));
        postSymbol = " ";
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
            Chunk chunk = new Chunk(preSymbol, symbol.getFont());
            switch (type ) {
                case 0:
                    chunk.append(String.valueOf((char)(first + list.size() + 171)));
                    break;
                case 1:
                    chunk.append(String.valueOf((char)(first + list.size() + 181)));
                    break;
                case 2:
                    chunk.append(String.valueOf((char)(first + list.size() + 191)));
                    break;
                default:
                    chunk.append(String.valueOf((char)(first + list.size() + 201)));
            }
            chunk.append(postSymbol);
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
