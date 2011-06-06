
package com.lowagie.text;

import java.util.ArrayList;

import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPCellEvent;
import com.lowagie.text.pdf.PdfPTable;


public class SimpleCell extends Rectangle implements PdfPCellEvent, TextElementArray {

    
    
    public static final boolean ROW = true;
    
    public static final boolean CELL = false;
    
    
    
    private ArrayList<Element> content = new ArrayList<Element>();
    
    private float width = 0f;
    
    private float widthpercentage = 0f;
    
    private float spacing_left = Float.NaN;
    
    private float spacing_right = Float.NaN;
    
    private float spacing_top = Float.NaN;
    
    private float spacing_bottom = Float.NaN;
    
    private float padding_left = Float.NaN;
    
    private float padding_right = Float.NaN;
    
    private float padding_top = Float.NaN;
    
    private float padding_bottom = Float.NaN;
    
    private int colspan = 1;
    
    private int horizontalAlignment = Element.ALIGN_UNDEFINED;
    
    private int verticalAlignment = Element.ALIGN_UNDEFINED;
    
    private boolean cellgroup = false;
    
    protected boolean useAscender = false;
    
    protected boolean useDescender = false;
    
    protected boolean useBorderPadding;
    
    
    public SimpleCell(boolean row) {
        super(0f, 0f, 0f, 0f);
        cellgroup = row;
        setBorder(BOX);
    }
    
    
    public void addElement(Element element) throws BadElementException {
        if (cellgroup) {
            if (element instanceof SimpleCell) {
                if(((SimpleCell)element).isCellgroup()) {
                    throw new BadElementException("You can't add one row to another row.");
                }
                content.add(element);
                return;
            }
            else {
                throw new BadElementException("You can only add cells to rows, no objects of type " + element.getClass().getName());
            }
        }
        if (element.type() == Element.PARAGRAPH
                || element.type() == Element.PHRASE
                || element.type() == Element.ANCHOR
                || element.type() == Element.CHUNK
                || element.type() == Element.LIST
                || element.type() == Element.MARKED
                || element.type() == Element.JPEG
                || element.type() == Element.JPEG2000
                || element.type() == Element.JBIG2
                || element.type() == Element.IMGRAW
                || element.type() == Element.IMGTEMPLATE) {
            content.add(element);
        }
        else {
            throw new BadElementException("You can't add an element of type " + element.getClass().getName() + " to a SimpleCell.");
        }
    }
    
    
    public Cell createCell(SimpleCell rowAttributes) throws BadElementException {
        Cell cell = new Cell();
        cell.cloneNonPositionParameters(rowAttributes);
        cell.softCloneNonPositionParameters(this);
        cell.setColspan(colspan);
        cell.setHorizontalAlignment(horizontalAlignment);
        cell.setVerticalAlignment(verticalAlignment);
        cell.setUseAscender(useAscender);
        cell.setUseBorderPadding(useBorderPadding);
        cell.setUseDescender(useDescender);
        for (Element element: content) {
            cell.addElement(element);
        }
        return cell;
    }
    
    
    public PdfPCell createPdfPCell(SimpleCell rowAttributes) {
        PdfPCell cell = new PdfPCell();
        cell.setBorder(NO_BORDER);
        SimpleCell tmp = new SimpleCell(CELL);
        tmp.setSpacing_left(spacing_left);
        tmp.setSpacing_right(spacing_right);
        tmp.setSpacing_top(spacing_top);
        tmp.setSpacing_bottom(spacing_bottom);
        tmp.cloneNonPositionParameters(rowAttributes);
        tmp.softCloneNonPositionParameters(this);
        cell.setCellEvent(tmp);
        cell.setHorizontalAlignment(rowAttributes.horizontalAlignment);
        cell.setVerticalAlignment(rowAttributes.verticalAlignment);
        cell.setUseAscender(rowAttributes.useAscender);
        cell.setUseBorderPadding(rowAttributes.useBorderPadding);
        cell.setUseDescender(rowAttributes.useDescender);
        cell.setColspan(colspan);
        if (horizontalAlignment != Element.ALIGN_UNDEFINED)
            cell.setHorizontalAlignment(horizontalAlignment);
        if (verticalAlignment != Element.ALIGN_UNDEFINED)
            cell.setVerticalAlignment(verticalAlignment);
        if (useAscender)
            cell.setUseAscender(useAscender);
        if (useBorderPadding)
            cell.setUseBorderPadding(useBorderPadding);
        if (useDescender)
            cell.setUseDescender(useDescender);
        float p;
        float sp_left = spacing_left;
        if (Float.isNaN(sp_left)) sp_left = 0f;
        float sp_right = spacing_right;
        if (Float.isNaN(sp_right)) sp_right = 0f;
        float sp_top = spacing_top;
        if (Float.isNaN(sp_top)) sp_top = 0f;
        float sp_bottom = spacing_bottom;
        if (Float.isNaN(sp_bottom)) sp_bottom = 0f;
        p = padding_left;
        if (Float.isNaN(p)) p = 0f; 
        cell.setPaddingLeft(p + sp_left);
        p = padding_right;
        if (Float.isNaN(p)) p = 0f; 
        cell.setPaddingRight(p + sp_right);
        p = padding_top;
        if (Float.isNaN(p)) p = 0f; 
        cell.setPaddingTop(p + sp_top);
        p = padding_bottom;
        if (Float.isNaN(p)) p = 0f; 
        cell.setPaddingBottom(p + sp_bottom);
        for (Element element: content) {
            cell.addElement(element);
        }
        return cell;
    }

    
    public void cellLayout(PdfPCell cell, Rectangle position, PdfContentByte[] canvases) {
        float sp_left = spacing_left;
        if (Float.isNaN(sp_left)) sp_left = 0f;
        float sp_right = spacing_right;
        if (Float.isNaN(sp_right)) sp_right = 0f;
        float sp_top = spacing_top;
        if (Float.isNaN(sp_top)) sp_top = 0f;
        float sp_bottom = spacing_bottom;
        if (Float.isNaN(sp_bottom)) sp_bottom = 0f;
        Rectangle rect = new Rectangle(position.getLeft(sp_left), position.getBottom(sp_bottom), position.getRight(sp_right), position.getTop(sp_top));
        rect.cloneNonPositionParameters(this);
        canvases[PdfPTable.BACKGROUNDCANVAS].rectangle(rect);
        rect.setBackgroundColor(null);
        canvases[PdfPTable.LINECANVAS].rectangle(rect);
    }
    
    
    public void setPadding(float padding) {
        if (Float.isNaN(padding_right)) {
            setPadding_right(padding);
        }
        if (Float.isNaN(padding_left)) {
            setPadding_left(padding);
        }
        if (Float.isNaN(padding_top)) {
            setPadding_top(padding);
        }
        if (Float.isNaN(padding_bottom)) {
            setPadding_bottom(padding);
        }
    }
    
    
    public int getColspan() {
        return colspan;
    }
    
    public void setColspan(int colspan) {
        if (colspan > 0) this.colspan = colspan;
    }
    
    public float getPadding_bottom() {
        return padding_bottom;
    }
    
    public void setPadding_bottom(float padding_bottom) {
        this.padding_bottom = padding_bottom;
    }
    
    public float getPadding_left() {
        return padding_left;
    }
    
    public void setPadding_left(float padding_left) {
        this.padding_left = padding_left;
    }
    
    public float getPadding_right() {
        return padding_right;
    }
    
    public void setPadding_right(float padding_right) {
        this.padding_right = padding_right;
    }
    
    public float getPadding_top() {
        return padding_top;
    }
    
    public void setPadding_top(float padding_top) {
        this.padding_top = padding_top;
    }
    
    public float getSpacing_left() {
        return spacing_left;
    }
    
    public float getSpacing_right() {
        return spacing_right;
    }
    
    public float getSpacing_top() {
        return spacing_top;
    }
    
    public float getSpacing_bottom() {
        return spacing_bottom;
    }
    
    
    public void setSpacing(float spacing) {
        this.spacing_left = spacing;
        this.spacing_right = spacing;
        this.spacing_top = spacing;
        this.spacing_bottom = spacing;
    }
    
    
    public void setSpacing_left(float spacing) {
        this.spacing_left = spacing;
    }
    
    
    public void setSpacing_right(float spacing) {
        this.spacing_right = spacing;
    }
    
    
    public void setSpacing_top(float spacing) {
        this.spacing_top = spacing;
    }
    
    
    public void setSpacing_bottom(float spacing) {
        this.spacing_bottom = spacing;
    }
    
    
    public boolean isCellgroup() {
        return cellgroup;
    }
    
    public void setCellgroup(boolean cellgroup) {
        this.cellgroup = cellgroup;
    }
    
    public int getHorizontalAlignment() {
        return horizontalAlignment;
    }
    
    public void setHorizontalAlignment(int horizontalAlignment) {
        this.horizontalAlignment = horizontalAlignment;
    }
    
    public int getVerticalAlignment() {
        return verticalAlignment;
    }
    
    public void setVerticalAlignment(int verticalAlignment) {
        this.verticalAlignment = verticalAlignment;
    }
    
    public float getWidth() {
        return width;
    }
    
    public void setWidth(float width) {
        this.width = width;
    }
    
    public float getWidthpercentage() {
        return widthpercentage;
    }
    
    public void setWidthpercentage(float widthpercentage) {
        this.widthpercentage = widthpercentage;
    }
    
    public boolean isUseAscender() {
        return useAscender;
    }
    
    public void setUseAscender(boolean useAscender) {
        this.useAscender = useAscender;
    }
    
    public boolean isUseBorderPadding() {
        return useBorderPadding;
    }
    
    public void setUseBorderPadding(boolean useBorderPadding) {
        this.useBorderPadding = useBorderPadding;
    }
    
    public boolean isUseDescender() {
        return useDescender;
    }
    
    public void setUseDescender(boolean useDescender) {
        this.useDescender = useDescender;
    }
    
    
    ArrayList<Element> getContent() {
        return content;
    }

    
    public boolean addObject(Object o) {
        try {
            addElement((Element)o);
            return true;
        }
        catch(ClassCastException e) {
            return false;
        }
        catch(BadElementException e) {
            throw new ExceptionConverter(e);
        }
    }

    
    public boolean add(Element element) {
        try {
            addElement(element);
            return true;
        }
        catch(ClassCastException e) {
            return false;
        }
        catch(BadElementException e) {
            throw new ExceptionConverter(e);
        }
    }
    
    public int type() {
        return Element.CELL;
    }
}
