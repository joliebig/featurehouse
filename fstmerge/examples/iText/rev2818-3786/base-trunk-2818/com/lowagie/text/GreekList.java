
package com.lowagie.text;

import com.lowagie.text.factories.GreekNumberFactory;



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
            Chunk chunk = new Chunk(GreekNumberFactory.getString(first + list.size(), lowercase), symbol.getFont());
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


    
    
    public static int[] getGreekValue(int index, boolean lowercase) {
           byte[] result = GreekNumberFactory.getString(index, lowercase).getBytes();
           int n = result.length;
           int[] r = new int[n];
           System.arraycopy(result, 0, r, 0, n);
           return r;
     }

    
    public void setGreekLower(boolean greeklower) {
        setLowercase(greeklower);
    }

    
    public boolean isGreekLower() {
        return isLowercase();
    }
}
