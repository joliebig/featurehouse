
package com.lowagie.text.pdf;
import java.awt.Color;
import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.Chunk;
import com.lowagie.text.Element;
import com.lowagie.text.Phrase;


public class VerticalText {

    
    public static final int NO_MORE_TEXT = 1;
    
    
    public static final int NO_MORE_COLUMN = 2;

    
    protected ArrayList chunks = new ArrayList();

        
    protected PdfContentByte text;
    
    
    protected int alignment = Element.ALIGN_LEFT;

    
    protected int currentChunkMarker = -1;
    
    
    protected PdfChunk currentStandbyChunk;
    
    
    protected String splittedChunkText;

        
    protected float leading;
    
        
    protected float startX;
    
        
    protected float startY;
    
        
    protected int maxLines;
    
        
    protected float height;
    
    
    public VerticalText(PdfContentByte text) {
        this.text = text;
    }
    
    
    public void addText(Phrase phrase) {
        for (Iterator j = phrase.getChunks().iterator(); j.hasNext();) {
            chunks.add(new PdfChunk((Chunk)j.next(), null));
        }
    }
    
    
    public void addText(Chunk chunk) {
        chunks.add(new PdfChunk(chunk, null));
    }

        
    public void setVerticalLayout(float startX, float startY, float height, int maxLines, float leading) {
        this.startX = startX;
        this.startY = startY;
        this.height = height;
        this.maxLines = maxLines;
        setLeading(leading);
    }
    
        
    public void setLeading(float leading) {
        this.leading = leading;
    }

        
    public float getLeading() {
        return leading;
    }
    
    
    protected PdfLine createLine(float width) {
        if (chunks.isEmpty())
            return null;
        splittedChunkText = null;
        currentStandbyChunk = null;
        PdfLine line = new PdfLine(0, width, alignment, 0);
        String total;
        for (currentChunkMarker = 0; currentChunkMarker < chunks.size(); ++currentChunkMarker) {
            PdfChunk original = (PdfChunk)(chunks.get(currentChunkMarker));
            total = original.toString();
            currentStandbyChunk = line.add(original);
            if (currentStandbyChunk != null) {
                splittedChunkText = original.toString();
                original.setValue(total);
                return line;
            }
        }
        return line;
    }
    
    
    protected void shortenChunkArray() {
        if (currentChunkMarker < 0)
            return;
        if (currentChunkMarker >= chunks.size()) {
            chunks.clear();
            return;
        }
        PdfChunk split = (PdfChunk)(chunks.get(currentChunkMarker));
        split.setValue(splittedChunkText);
        chunks.set(currentChunkMarker, currentStandbyChunk);
        for (int j = currentChunkMarker - 1; j >= 0; --j)
            chunks.remove(j);
    }

    
    public int go() {
        return go(false);
    }
    
    
    public int go(boolean simulate) {
        boolean dirty = false;
        PdfContentByte graphics = null;
        if (text != null) {
            graphics = text.getDuplicate();
        }
        else if (!simulate)
            throw new NullPointerException("VerticalText.go with simulate==false and text==null.");
        int status = 0;
        for (;;) {
            if (maxLines <= 0) {
                status = NO_MORE_COLUMN;
                if (chunks.isEmpty())
                    status |= NO_MORE_TEXT;
                break;
            }
            if (chunks.isEmpty()) {
                status = NO_MORE_TEXT;
                break;
            }
            PdfLine line = createLine(height);
            if (!simulate && !dirty) {
                text.beginText();
                dirty = true;
            }
            shortenChunkArray();
            if (!simulate) {
                text.setTextMatrix(startX, startY - line.indentLeft());
                writeLine(line, text, graphics);
            }
            --maxLines;
            startX -= leading;
        }
        if (dirty) {
            text.endText();
            text.add(graphics);
        }
        return status;
    }
    
    void writeLine(PdfLine line, PdfContentByte text, PdfContentByte graphics) {
        PdfFont currentFont = null;
        PdfChunk chunk;
        for (Iterator j = line.iterator(); j.hasNext(); ) {
            chunk = (PdfChunk) j.next();
            
            if (chunk.font().compareTo(currentFont) != 0) {
                currentFont = chunk.font();
                text.setFontAndSize(currentFont.getFont(), currentFont.size());
            }
            Color color = chunk.color();
            if (color != null)
                text.setColorFill(color);
            text.showText(chunk.toString());
            if (color != null)
                text.resetRGBColorFill();
        }
    }
    
        
    public void setOrigin(float startX, float startY) {
        this.startX = startX;
        this.startY = startY;
    }
    
        
    public float getOriginX() {
        return startX;
    }

        
    public float getOriginY() {
        return startY;
    }
    
    
    public int getMaxLines() {
        return maxLines;
    }
    
    
    public void setMaxLines(int maxLines) {
        this.maxLines = maxLines;
    }
    
    
    public float getHeight() {
        return height;
    }
    
    
    public void setHeight(float height) {
        this.height = height;
    }
    
    
    public void setAlignment(int alignment) {
        this.alignment = alignment;
    }
    
    
    public int getAlignment() {
        return alignment;
    }
}
