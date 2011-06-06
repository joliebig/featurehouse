

package com.lowagie.text.pdf;

import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.Anchor;
import com.lowagie.text.Cell;
import com.lowagie.text.Chunk;
import com.lowagie.text.Element;
import com.lowagie.text.Image;
import com.lowagie.text.List;
import com.lowagie.text.ListItem;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;



public class PdfCell extends Rectangle {

    
    
    
    private ArrayList lines;

    
    private PdfLine line;

    
    private ArrayList images;

    
    private float leading;

    
    private int rownumber;

    
    private int rowspan;

    
    private float cellspacing;

    
    private float cellpadding;

    
    private boolean header = false;

    
    private float contentHeight = 0.0f;

    
    private boolean useAscender;

    
    private boolean useDescender;

    
    private boolean useBorderPadding;

    private int verticalAlignment;

    private PdfLine firstLine;
    private PdfLine lastLine;

    
    
    

    public PdfCell(Cell cell, int rownumber, float left, float right, float top, float cellspacing, float cellpadding) {
        
        super(left, top, right, top);
        
        cloneNonPositionParameters(cell);
        this.cellpadding = cellpadding;
        this.cellspacing = cellspacing;
        this.verticalAlignment = cell.getVerticalAlignment();
        this.useAscender = cell.isUseAscender();
        this.useDescender = cell.isUseDescender();
        this.useBorderPadding = cell.isUseBorderPadding();

        
        PdfChunk chunk;
        Element element;
        PdfChunk overflow;
        lines = new ArrayList();
        images = new ArrayList();
        leading = cell.getLeading();
        int alignment = cell.getHorizontalAlignment();
        left += cellspacing + cellpadding;
        right -= cellspacing + cellpadding;

        left += getBorderWidthInside(LEFT);
        right -= getBorderWidthInside(RIGHT);


        contentHeight = 0;

        rowspan = cell.getRowspan();

        ArrayList allActions;
        int aCounter;
        
        for (Iterator i = cell.getElements(); i.hasNext();) {
            element = (Element) i.next();
            switch (element.type()) {
                case Element.JPEG:
                case Element.JPEG2000:
                case Element.IMGRAW:
                case Element.IMGTEMPLATE:
                    addImage((Image) element, left, right, 0.4f * leading, alignment); 
                    break;
                    
                case Element.LIST:
                    if (line != null && line.size() > 0) {
                        line.resetAlignment();
                        addLine(line);
                    }
                    
                    addList((List)element, left, right, alignment);
                    line = new PdfLine(left, right, alignment, leading);
                    break;
                    
                default:
                    allActions = new ArrayList();
                    processActions(element, null, allActions);
                    aCounter = 0;

                    float currentLineLeading = leading;
                    float currentLeft = left;
                    float currentRight = right;
                    if (element instanceof Phrase) {
                        currentLineLeading = ((Phrase) element).getLeading();
                    }
                    if (element instanceof Paragraph) {
                        Paragraph p = (Paragraph) element;
                        currentLeft += p.getIndentationLeft();
                        currentRight -= p.getIndentationRight();
                    }
                    if (line == null) {
                        line = new PdfLine(currentLeft, currentRight, alignment, currentLineLeading);
                    }
                    
                    ArrayList chunks = element.getChunks();
                    if (chunks.isEmpty()) {
                       addLine(line); 
                       line = new PdfLine(currentLeft, currentRight, alignment, currentLineLeading);
                    }
                    else {
                        for (Iterator j = chunks.iterator(); j.hasNext();) {
                            Chunk c = (Chunk) j.next();
                            chunk = new PdfChunk(c, (PdfAction) (allActions.get(aCounter++)));
                            while ((overflow = line.add(chunk)) != null) {
                                addLine(line);
                                line = new PdfLine(currentLeft, currentRight, alignment, currentLineLeading);
                                chunk = overflow;
                            }
                        }
                    }
                    
                    switch (element.type()) {
                        case Element.PARAGRAPH:
                        case Element.SECTION:
                        case Element.CHAPTER:
                            line.resetAlignment();
                            flushCurrentLine();
                    }
            }
        }
        flushCurrentLine();
        if (lines.size() > cell.getMaxLines()) {
            while (lines.size() > cell.getMaxLines()) {
                removeLine(lines.size() - 1);
            }
            if (cell.getMaxLines() > 0) {
                String more = cell.getShowTruncation();
                if (more != null && more.length() > 0) {
                    
                    lastLine = (PdfLine) lines.get(lines.size() - 1);
                    if (lastLine.size() >= 0) {
                        PdfChunk lastChunk = lastLine.getChunk(lastLine.size() - 1);
                        float moreWidth = new PdfChunk(more, lastChunk).width();
                        while (lastChunk.toString().length() > 0 && lastChunk.width() + moreWidth > right - left) {
                            
                            lastChunk.setValue(lastChunk.toString().substring(0, lastChunk.length() - 1));
                        }
                        lastChunk.setValue(lastChunk.toString() + more);
                    } else {
                        lastLine.add(new PdfChunk(new Chunk(more), null));
                    }
                }
            }
        }
        
        if (useDescender && lastLine != null) {
            contentHeight -= lastLine.getDescender();
        }

        
        if (!lines.isEmpty()) {
            firstLine = (PdfLine) lines.get(0);
            float firstLineRealHeight = firstLineRealHeight();
            contentHeight -= firstLine.height();
            firstLine.height = firstLineRealHeight;
            contentHeight += firstLineRealHeight;
        }

        float newBottom = top - contentHeight - (2f * cellpadding()) - (2f * cellspacing());
        newBottom -= getBorderWidthInside(TOP) + getBorderWidthInside(BOTTOM);
        setBottom(newBottom);

        this.rownumber = rownumber;
    }

    private void addList(List list, float left, float right, int alignment) {
        PdfChunk chunk;
        PdfChunk overflow;
        ArrayList allActions = new ArrayList();
        processActions(list, null, allActions);
        int aCounter = 0;
        for (Iterator it = list.getItems().iterator(); it.hasNext();) {
            Element ele = (Element)it.next();
            switch (ele.type()) {
                case Element.LISTITEM:
                    ListItem item = (ListItem)ele;
                    line = new PdfLine(left + item.getIndentationLeft(), right, alignment, item.getLeading());
                    line.setListItem(item);
                    for (Iterator j = item.getChunks().iterator(); j.hasNext();) {
                        chunk = new PdfChunk((Chunk) j.next(), (PdfAction) (allActions.get(aCounter++)));
                        while ((overflow = line.add(chunk)) != null) {
                            addLine(line);
                            line = new PdfLine(left + item.getIndentationLeft(), right, alignment, item.getLeading());
                            chunk = overflow;
                        }
                        line.resetAlignment();
                        addLine(line);
                        line = new PdfLine(left + item.getIndentationLeft(), right, alignment, leading);
                    }
                    break;
                case Element.LIST:
                    List sublist = (List)ele;
                    addList(sublist, left + sublist.getIndentationLeft(), right, alignment);
                    break;
            }
        }
    }

    


    
    public void setBottom(float value) {
        super.setBottom(value);
        float firstLineRealHeight = firstLineRealHeight();

        float totalHeight = ury - value; 
        float nonContentHeight = (cellpadding() * 2f) + (cellspacing() * 2f);
        nonContentHeight += getBorderWidthInside(TOP) + getBorderWidthInside(BOTTOM);

        float interiorHeight = totalHeight - nonContentHeight;
        float extraHeight = 0.0f;

        switch (verticalAlignment) {
            case Element.ALIGN_BOTTOM:
                extraHeight = interiorHeight - contentHeight;
                break;
            case Element.ALIGN_MIDDLE:
                extraHeight = (interiorHeight - contentHeight) / 2.0f;
                break;
            default:    
                extraHeight = 0f;
        }

        extraHeight += cellpadding() + cellspacing();
        extraHeight += getBorderWidthInside(TOP);
        if (firstLine != null) {
            firstLine.height = firstLineRealHeight + extraHeight;
        }
    }

    
    
    public float left() {
        return getLeft();
    }




    

    public float getLeft() {
        return super.getLeft(cellspacing);
    }

    
    
    public float right() {
        return getRight();
    }




    

    public float getRight() {
        return super.getRight(cellspacing);
    }

    
    
    public float top() {
        return getTop();
    }




    

    public float getTop() {
        return super.getTop(cellspacing);
    }

    
    
    public float bottom() {
        return getBottom();
    }




    

    public float getBottom() {
        return super.getBottom(cellspacing);
    }
    
    

    private void addLine(PdfLine line) {
        lines.add(line);
        contentHeight += line.height();
        lastLine = line;
        this.line = null;
    }

    private PdfLine removeLine(int index) {
        PdfLine oldLine = (PdfLine) lines.remove(index);
        contentHeight -= oldLine.height();
        if (index == 0) {
            if (!lines.isEmpty()) {
                firstLine = (PdfLine) lines.get(0);
                float firstLineRealHeight = firstLineRealHeight();
                contentHeight -= firstLine.height();
                firstLine.height = firstLineRealHeight;
                contentHeight += firstLineRealHeight;
            }
        }
        return oldLine;
    }

    private void flushCurrentLine() {
        if (line != null && line.size() > 0) {
            addLine(line);
        }
    }

    
    private float firstLineRealHeight() {
        float firstLineRealHeight = 0f;
        if (firstLine != null) {
            PdfChunk chunk = firstLine.getChunk(0);
            if (chunk != null) {
                Image image = chunk.getImage();
                if (image != null) {
                    firstLineRealHeight = firstLine.getChunk(0).getImage().getScaledHeight();
                } else {
                    firstLineRealHeight = useAscender ? firstLine.getAscender() : leading;
                }
            }
        }
        return firstLineRealHeight;
    }

    
    private float getBorderWidthInside(int side) {
        float width = 0f;
        if (useBorderPadding) {
            switch (side) {
                case Rectangle.LEFT:
                    width = getBorderWidthLeft();
                    break;

                case Rectangle.RIGHT:
                    width = getBorderWidthRight();
                    break;

                case Rectangle.TOP:
                    width = getBorderWidthTop();
                    break;

                default:    
                    width = getBorderWidthBottom();
                    break;
            }
            
            if (!isUseVariableBorders()) {
                width = width / 2f;
            }
        }
        return width;
    }


    

    private float addImage(Image i, float left, float right, float extraHeight, int alignment) {
        Image image = Image.getInstance(i);
        if (image.getScaledWidth() > right - left) {
            image.scaleToFit(right - left, Float.MAX_VALUE);
        }
        flushCurrentLine();
        if (line == null) {
            line = new PdfLine(left, right, alignment, leading);
        }
        PdfLine imageLine = line;

        
        right = right - left;
        left = 0f;

        if ((image.getAlignment() & Image.RIGHT) == Image.RIGHT) {
            left = right - image.getScaledWidth();
        } else if ((image.getAlignment() & Image.MIDDLE) == Image.MIDDLE) {
            left = left + ((right - left - image.getScaledWidth()) / 2f);
        }
        Chunk imageChunk = new Chunk(image, left, 0);
        imageLine.add(new PdfChunk(imageChunk, null));
        addLine(imageLine);
        return imageLine.height();
    }

    

    public ArrayList getLines(float top, float bottom) {
        float lineHeight;
        float currentPosition = Math.min(getTop(), top);
        setTop(currentPosition + cellspacing);
        ArrayList result = new ArrayList();

        
        if (getTop() < bottom) {
            return result;
        }
        
        
        int size = lines.size();
        boolean aboveBottom = true;
        for (int i = 0; i < size && aboveBottom; i++) {
            line = (PdfLine) lines.get(i);
            lineHeight = line.height();
            currentPosition -= lineHeight;
            
            if (currentPosition > (bottom + cellpadding + getBorderWidthInside(BOTTOM))) {
                result.add(line);
            } else {
                aboveBottom = false;
            }
        }
        
        float difference = 0f;
        if (!header) {
            if (aboveBottom) {
                lines = new ArrayList();
                contentHeight = 0f;
            } else {
                size = result.size();
                for (int i = 0; i < size; i++) {
                    line = removeLine(0);
                    difference += line.height();
                }
            }
        }
        if (difference > 0) {
            Image image;
            for (Iterator i = images.iterator(); i.hasNext();) {
                image = (Image) i.next();
                image.setAbsolutePosition(image.getAbsoluteX(), image.getAbsoluteY() - difference - leading);
            }
        }
        return result;
    }

    

    public ArrayList getImages(float top, float bottom) {

        
        if (getTop() < bottom) {
            return new ArrayList();
        }
        top = Math.min(getTop(), top);
        
        Image image;
        float height;
        ArrayList result = new ArrayList();
        
        for (Iterator i = images.iterator(); i.hasNext() && !header;) {
            image = (Image) i.next();
            height = image.getAbsoluteY();
            
            if (top - height > (bottom + cellpadding)) {
                image.setAbsolutePosition(image.getAbsoluteX(), top - height);
                result.add(image);
                i.remove();
            }
        }
        return result;
    }

    

    boolean isHeader() {
        return header;
    }

    

    void setHeader() {
        header = true;
    }

    

    boolean mayBeRemoved() {
        return (header || (lines.isEmpty() && images.isEmpty()));
    }

    

    public int size() {
        return lines.size();
    }

    
    private float remainingLinesHeight() {
        if (lines.isEmpty()) return 0;
        float result = 0;
        int size = lines.size();
        PdfLine line;
        for (int i = 0; i < size; i++) {
            line = (PdfLine) lines.get(i);
            result += line.height();
        }
        return result;
    }

    

    public float remainingHeight() {
        float result = 0f;
        for (Iterator i = images.iterator(); i.hasNext();) {
            Image image = (Image) i.next();
            result += image.getScaledHeight();
        }
        return remainingLinesHeight() + cellspacing + 2 * cellpadding + result;
    }
    
    
    
    

    public float leading() {
        return leading;
    }

    

    public int rownumber() {
        return rownumber;
    }

    

    public int rowspan() {
        return rowspan;
    }

    

    public float cellspacing() {
        return cellspacing;
    }

    

    public float cellpadding() {
        return cellpadding;
    }

    

    protected void processActions(Element element, PdfAction action, ArrayList allActions) {
        if (element.type() == Element.ANCHOR) {
            String url = ((Anchor) element).getReference();
            if (url != null) {
                action = new PdfAction(url);
            }
        }
        Iterator i;
        switch (element.type()) {
            case Element.PHRASE:
            case Element.SECTION:
            case Element.ANCHOR:
            case Element.CHAPTER:
            case Element.LISTITEM:
            case Element.PARAGRAPH:
                for (i = ((ArrayList) element).iterator(); i.hasNext();) {
                    processActions((Element) i.next(), action, allActions);
                }
                break;
            case Element.CHUNK:
                allActions.add(action);
                break;
            case Element.LIST:
                for (i = ((List) element).getItems().iterator(); i.hasNext();) {
                    processActions((Element) i.next(), action, allActions);
                }
                break;
            default:
                int n = element.getChunks().size();
                while (n-- > 0)
                    allActions.add(action);
                break;
        }
    }

    
    private int groupNumber;

    

    public int getGroupNumber() {
        return groupNumber;
    }

    

    void setGroupNumber(int number) {
        groupNumber = number;
    }

    

    public Rectangle rectangle(float top, float bottom) {
        Rectangle tmp = new Rectangle(getLeft(), getBottom(), getRight(), getTop());
        tmp.cloneNonPositionParameters(this);
        if (getTop() > top) {
            tmp.setTop(top);
            tmp.setBorder(border - (border & TOP));
        }
        if (getBottom() < bottom) {
            tmp.setBottom(bottom);
            tmp.setBorder(border - (border & BOTTOM));
        }
        return tmp;
    }

    
    public void setUseAscender(boolean use) {
        useAscender = use;
    }

    
    public boolean isUseAscender() {
        return useAscender;
    }

    
    public void setUseDescender(boolean use) {
        useDescender = use;
    }

    
    public boolean isUseDescender() {
        return useDescender;
    }

    
    public void setUseBorderPadding(boolean use) {
        useBorderPadding = use;
    }

    
    public boolean isUseBorderPadding() {
        return useBorderPadding;
    }

}
