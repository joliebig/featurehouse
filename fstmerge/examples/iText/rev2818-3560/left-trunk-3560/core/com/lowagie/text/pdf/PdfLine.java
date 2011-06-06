

package com.lowagie.text.pdf;

import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.Chunk;
import com.lowagie.text.Element;
import com.lowagie.text.ListItem;



public class PdfLine {
    
    
    
    
    protected ArrayList line;
    
    
    protected float left;
    
    
    protected float width;
    
    
    protected int alignment;
    
    
    protected float height;
    
    
    protected Chunk listSymbol = null;
    
    
    protected float symbolIndent;
    
    
    protected boolean newlineSplit = false;
    
    
    protected float originalWidth;
    
    protected boolean isRTL = false;
    
    
    
    
    
    PdfLine(float left, float right, int alignment, float height) {
        this.left = left;
        this.width = right - left;
        this.originalWidth = this.width;
        this.alignment = alignment;
        this.height = height;
        this.line = new ArrayList();
    }
    
    
    PdfLine(float left, float originalWidth, float remainingWidth, int alignment, boolean newlineSplit, ArrayList line, boolean isRTL) {
        this.left = left;
        this.originalWidth = originalWidth;
        this.width = remainingWidth;
        this.alignment = alignment;
        this.line = line;
        this.newlineSplit = newlineSplit;
        this.isRTL = isRTL;
    }
    
    
    
    
    
    PdfChunk add(PdfChunk chunk) {
        
        if (chunk == null || chunk.toString().equals("")) {
            return null;
        }
        
        
        PdfChunk overflow = chunk.split(width);
        newlineSplit = (chunk.isNewlineSplit() || overflow == null);
        
        
        if (chunk.isTab()) {
            Object[] tab = (Object[])chunk.getAttribute(Chunk.TAB);
            float tabPosition = ((Float)tab[1]).floatValue();
            boolean newline = ((Boolean)tab[2]).booleanValue();
            if (newline && tabPosition < originalWidth - width) {
                return chunk;
            }
            width = originalWidth - tabPosition;
            chunk.adjustLeft(left);
            addToLine(chunk);
        }
        
        else if (chunk.length() > 0) {
            if (overflow != null)
                chunk.trimLastSpace();
            width -= chunk.width();
            addToLine(chunk);
        }
        
        
        else if (line.size() < 1) {
            chunk = overflow;
            overflow = chunk.truncate(width);
            width -= chunk.width();
            if (chunk.length() > 0) {
                addToLine(chunk);
                return overflow;
            }
            
            else {
                if (overflow != null)
                    addToLine(overflow);
                return null;
            }
        }
        else {
            width += ((PdfChunk)(line.get(line.size() - 1))).trimLastSpace();
        }
        return overflow;
    }
    
    private void addToLine(PdfChunk chunk) {
        if (chunk.changeLeading && chunk.isImage()) {
            float f = chunk.getImage().getScaledHeight() + chunk.getImageOffsetY();
            if (f > height) height = f;
        }
        line.add(chunk);
    }
    
    
    
    
    
    public int size() {
        return line.size();
    }
    
    
    
    public Iterator iterator() {
        return line.iterator();
    }
    
    
    
    float height() {
        return height;
    }
    
    
    
    float indentLeft() {
        if (isRTL) {
            switch (alignment) {
                case Element.ALIGN_LEFT:
                    return left + width;
                case Element.ALIGN_CENTER:
                    return left + (width / 2f);
                default:
                    return left;
            }
        }
        else {
            switch (alignment) {
                case Element.ALIGN_RIGHT:
                    return left + width;
                case Element.ALIGN_CENTER:
                    return left + (width / 2f);
                default:
                    return left;
            }
        }
    }
    
    
    
    public boolean hasToBeJustified() {
        return ((alignment == Element.ALIGN_JUSTIFIED || alignment == Element.ALIGN_JUSTIFIED_ALL) && width != 0);
    }
    
    
    
    public void resetAlignment() {
        if (alignment == Element.ALIGN_JUSTIFIED) {
            alignment = Element.ALIGN_LEFT;
        }
    }
    
    
    void setExtraIndent(float extra) {
        left += extra;
        width -= extra;
    }
    
    
    
    float widthLeft() {
        return width;
    }
    
    
    
    int numberOfSpaces() {
        String string = toString();
        int length = string.length();
        int numberOfSpaces = 0;
        for (int i = 0; i < length; i++) {
            if (string.charAt(i) == ' ') {
                numberOfSpaces++;
            }
        }
        return numberOfSpaces;
    }
    
    
    
    public void setListItem(ListItem listItem) {
        this.listSymbol = listItem.getListSymbol();
        this.symbolIndent = listItem.getIndentationLeft();
    }
    
    
    
    public Chunk listSymbol() {
        return listSymbol;
    }
    
    
    
    public float listIndent() {
        return symbolIndent;
    }
    
    
    
    public String toString() {
        StringBuffer tmp = new StringBuffer();
        for (Iterator i = line.iterator(); i.hasNext(); ) {
            tmp.append(((PdfChunk) i.next()).toString());
        }
        return tmp.toString();
    }
    
    
    public int GetLineLengthUtf32() {
        int total = 0;
        for (Iterator i = line.iterator(); i.hasNext();) {
            total += ((PdfChunk)i.next()).lengthUtf32();
        }
        return total;
    }
    
    
    public boolean isNewlineSplit() {
        return newlineSplit && (alignment != Element.ALIGN_JUSTIFIED_ALL);
    }
    
    
    public int getLastStrokeChunk() {
        int lastIdx = line.size() - 1;
        for (; lastIdx >= 0; --lastIdx) {
            PdfChunk chunk = (PdfChunk)line.get(lastIdx);
            if (chunk.isStroked())
                break;
        }
        return lastIdx;
    }
    
    
    public PdfChunk getChunk(int idx) {
        if (idx < 0 || idx >= line.size())
            return null;
        return (PdfChunk)line.get(idx);
    }
    
    
    public float getOriginalWidth() {
        return originalWidth;
    }
    
    
    float getMaxSizeSimple() {
        float maxSize = 0;
        for (int k = 0; k < line.size(); ++k) {
            PdfChunk chunk = (PdfChunk)line.get(k);
            if (!chunk.isImage()) {
                maxSize = Math.max(chunk.font().size(), maxSize);
            }
            else {
                maxSize = Math.max(chunk.getImage().getScaledHeight() + chunk.getImageOffsetY() , maxSize);
            }
        }
        return maxSize;
    }
    
    boolean isRTL() {
        return isRTL;
    }
    
    
    int getSeparatorCount() {
        int s = 0;
        PdfChunk ck;
        for (Iterator i = line.iterator(); i.hasNext(); ) {
            ck = (PdfChunk)i.next();
            if (ck.isTab()) {
                return 0;
            }
            if (ck.isHorizontalSeparator()) {
                s++;
            }
        }
        return s;
    }
    
    
    public float getWidthCorrected(float charSpacing, float wordSpacing) {
        float total = 0;
        for (int k = 0; k < line.size(); ++k) {
            PdfChunk ck = (PdfChunk)line.get(k);
            total += ck.getWidthCorrected(charSpacing, wordSpacing);
        }
        return total;
    }
    

   public float getAscender() {
       float ascender = 0;
       for (int k = 0; k < line.size(); ++k) {
           PdfChunk ck = (PdfChunk)line.get(k);
           if (ck.isImage())
               ascender = Math.max(ascender, ck.getImage().getScaledHeight() + ck.getImageOffsetY());
           else {
               PdfFont font = ck.font();
               ascender = Math.max(ascender, font.getFont().getFontDescriptor(BaseFont.ASCENT, font.size()));
           }
       }
       return ascender;
   }


    public float getDescender() {
        float descender = 0;
        for (int k = 0; k < line.size(); ++k) {
            PdfChunk ck = (PdfChunk)line.get(k);
            if (ck.isImage())
                descender = Math.min(descender, ck.getImageOffsetY());
            else {
                PdfFont font = ck.font();
                descender = Math.min(descender, font.getFont().getFontDescriptor(BaseFont.DESCENT, font.size()));
            }
        }
        return descender;
    }
}