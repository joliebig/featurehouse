
package com.lowagie.text;

import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfPTable;
import com.lowagie.text.pdf.PdfPTableEvent;


public class SimpleTable extends Rectangle implements PdfPTableEvent, TextElementArray {

    
    private ArrayList content = new ArrayList();
    
    private float width = 0f;
    
    private float widthpercentage = 0f;
    
    private float cellspacing;
    
    private float cellpadding;
    
    private int alignment;
    
    
    public SimpleTable() {
        super(0f, 0f, 0f, 0f);
        setBorder(BOX);
        setBorderWidth(2f);
    }
    
    
    public void addElement(SimpleCell element) throws BadElementException {
        if(!element.isCellgroup()) {
            throw new BadElementException("You can't add cells to a table directly, add them to a row first.");
        }
        content.add(element);
    }
    
    
    public Table createTable() throws BadElementException {
        if (content.isEmpty()) throw new BadElementException("Trying to create a table without rows.");
        SimpleCell row = (SimpleCell)content.get(0);
        SimpleCell cell;
        int columns = 0;
        for (Iterator i = row.getContent().iterator(); i.hasNext(); ) {
            cell = (SimpleCell)i.next();
            columns += cell.getColspan();
        }
        float[] widths = new float[columns];
        float[] widthpercentages = new float[columns];
        Table table = new Table(columns);
        table.setAlignment(alignment);
        table.setSpacing(cellspacing);
        table.setPadding(cellpadding);
        table.cloneNonPositionParameters(this);
        int pos;
        for (Iterator rows = content.iterator(); rows.hasNext(); ) {
            row = (SimpleCell)rows.next();
            pos = 0;
            for (Iterator cells = row.getContent().iterator(); cells.hasNext(); ) {
                cell = (SimpleCell)cells.next();
                table.addCell(cell.createCell(row));
                if (cell.getColspan() == 1) {
                    if (cell.getWidth() > 0) widths[pos] = cell.getWidth();
                    if (cell.getWidthpercentage() > 0) widthpercentages[pos] = cell.getWidthpercentage();
                }
                pos += cell.getColspan();
            }
        }
        float sumWidths = 0f;
        for(int i = 0; i < columns; i++) {
            if (widths[i] == 0) {
                sumWidths = 0;
                break;
            }
            sumWidths += widths[i];
        }
        if (sumWidths > 0) {
            table.setWidth(sumWidths);
            table.setLocked(true);
            table.setWidths(widths);
        }
        else {
            for(int i = 0; i < columns; i++) {
                if (widthpercentages[i] == 0) {
                    sumWidths = 0;
                    break;
                }
                sumWidths += widthpercentages[i];
            }
            if (sumWidths > 0) {
                table.setWidths(widthpercentages);
            }
        }
        if (width > 0) {
            table.setWidth(width);
            table.setLocked(true);
        }
        else if (widthpercentage > 0) {
            table.setWidth(widthpercentage);
        }
        return table;
    }
    
    
    public PdfPTable createPdfPTable() throws DocumentException {
        if (content.isEmpty()) throw new BadElementException("Trying to create a table without rows.");
        SimpleCell row = (SimpleCell)content.get(0);
        SimpleCell cell;
        int columns = 0;
        for (Iterator i = row.getContent().iterator(); i.hasNext(); ) {
            cell = (SimpleCell)i.next();
            columns += cell.getColspan();
        }
        float[] widths = new float[columns];
        float[] widthpercentages = new float[columns];
        PdfPTable table = new PdfPTable(columns);
        table.setTableEvent(this);
        table.setHorizontalAlignment(alignment);
        int pos;
        for (Iterator rows = content.iterator(); rows.hasNext(); ) {
            row = (SimpleCell)rows.next();
            pos = 0;
            for (Iterator cells = row.getContent().iterator(); cells.hasNext(); ) {
                cell = (SimpleCell)cells.next();
                if (Float.isNaN(cell.getSpacing_left()))    {
                    cell.setSpacing_left(cellspacing / 2f);
                }
                if (Float.isNaN(cell.getSpacing_right()))    {
                    cell.setSpacing_right(cellspacing / 2f);
                }
                if (Float.isNaN(cell.getSpacing_top()))    {
                    cell.setSpacing_top(cellspacing / 2f);
                }
                if (Float.isNaN(cell.getSpacing_bottom()))    {
                    cell.setSpacing_bottom(cellspacing / 2f);
                }
                cell.setPadding(cellpadding);
                table.addCell(cell.createPdfPCell(row));
                if (cell.getColspan() == 1) {
                    if (cell.getWidth() > 0) widths[pos] = cell.getWidth();
                    if (cell.getWidthpercentage() > 0) widthpercentages[pos] = cell.getWidthpercentage();
                }
                pos += cell.getColspan();
            }
        }
        float sumWidths = 0f;
        for(int i = 0; i < columns; i++) {
            if (widths[i] == 0) {
                sumWidths = 0;
                break;
            }
            sumWidths += widths[i];
        }
        if (sumWidths > 0) {
            table.setTotalWidth(sumWidths);
            table.setWidths(widths);
        }
        else {
            for(int i = 0; i < columns; i++) {
                if (widthpercentages[i] == 0) {
                    sumWidths = 0;
                    break;
                }
                sumWidths += widthpercentages[i];
            }
            if (sumWidths > 0) {
                table.setWidths(widthpercentages);
            }
        }
        if (width > 0) {
            table.setTotalWidth(width);
        }
        if (widthpercentage > 0) {
            table.setWidthPercentage(widthpercentage);
        }
        return table;
    }
    
    
    public static SimpleTable getDimensionlessInstance(Rectangle rectangle, float spacing) {
        SimpleTable event = new SimpleTable();
        event.cloneNonPositionParameters(rectangle);
        event.setCellspacing(spacing);
        return event;
    }
    
    
    public void tableLayout(PdfPTable table, float[][] widths, float[] heights, int headerRows, int rowStart, PdfContentByte[] canvases) {
        float[] width = widths[0];
        Rectangle rect = new Rectangle(width[0], heights[heights.length - 1], width[width.length - 1], heights[0]);
        rect.cloneNonPositionParameters(this);
        int bd = rect.getBorder();
        rect.setBorder(Rectangle.NO_BORDER);
        canvases[PdfPTable.BACKGROUNDCANVAS].rectangle(rect);
        rect.setBorder(bd);
        rect.setBackgroundColor(null);
        canvases[PdfPTable.LINECANVAS].rectangle(rect);
    }
    
    
    public float getCellpadding() {
        return cellpadding;
    }
    
    public void setCellpadding(float cellpadding) {
        this.cellpadding = cellpadding;
    }
    
    public float getCellspacing() {
        return cellspacing;
    }
    
    public void setCellspacing(float cellspacing) {
        this.cellspacing = cellspacing;
    }
    
    
    public int getAlignment() {
        return alignment;
    }
    
    public void setAlignment(int alignment) {
        this.alignment = alignment;
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
    
    public int type() {
        return Element.TABLE;
    }

    
    public boolean isNestable() {
        return true;
    }

    
    public boolean add(Object o) {
        try {
            addElement((SimpleCell)o);
            return true;
        }
        catch(ClassCastException e) {
            return false;
        }
        catch(BadElementException e) {
            throw new ExceptionConverter(e);
        }
    }
}