

package com.lowagie.text.pdf;

import java.util.ArrayList;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.ElementListener;
import com.lowagie.text.Image;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.pdf.events.PdfPTableEventForwarder;



public class PdfPTable implements Element{
    
        
    public static final int BASECANVAS = 0;
        
    public static final int BACKGROUNDCANVAS = 1;
        
    public static final int LINECANVAS = 2;
        
    public static final int TEXTCANVAS = 3;
    
    protected ArrayList rows = new ArrayList();
    protected float totalHeight = 0;
    protected PdfPCell currentRow[];
    protected int currentRowIdx = 0;
    protected PdfPCell defaultCell = new PdfPCell((Phrase)null);
    protected float totalWidth = 0;
    protected float relativeWidths[];
    protected float absoluteWidths[];
    protected PdfPTableEvent tableEvent;
    

    protected int headerRows;
    

    protected float widthPercentage = 80;
    

    private int horizontalAlignment = Element.ALIGN_CENTER;
    

    private boolean skipFirstHeader = false;

    protected boolean isColspan = false;
    
    protected int runDirection = PdfWriter.RUN_DIRECTION_DEFAULT;

    
    private boolean lockedWidth = false;
    
    
    private boolean splitRows = true;
    

    protected float spacingBefore;
    

    protected float spacingAfter;
    
    
    private boolean extendLastRow;
    
    
    private boolean headersInEvent;
    
    
    private boolean splitLate = true;
    
    
    private boolean keepTogether;
    
    
    private int footerRows;
    
    protected PdfPTable() {
    }
    
        
    public PdfPTable(float relativeWidths[]) {
        if (relativeWidths == null)
            throw new NullPointerException("The widths array in PdfPTable constructor can not be null.");
        if (relativeWidths.length == 0)
            throw new IllegalArgumentException("The widths array in PdfPTable constructor can not have zero length.");
        this.relativeWidths = new float[relativeWidths.length];
        System.arraycopy(relativeWidths, 0, this.relativeWidths, 0, relativeWidths.length);
        absoluteWidths = new float[relativeWidths.length];
        calculateWidths();
        currentRow = new PdfPCell[absoluteWidths.length];
        keepTogether = false;
    }
    
        
    public PdfPTable(int numColumns) {
        if (numColumns <= 0)
            throw new IllegalArgumentException("The number of columns in PdfPTable constructor must be greater than zero.");
        relativeWidths = new float[numColumns];
        for (int k = 0; k < numColumns; ++k)
            relativeWidths[k] = 1;
        absoluteWidths = new float[relativeWidths.length];
        calculateWidths();
        currentRow = new PdfPCell[absoluteWidths.length];
        keepTogether = false;
    }
    
        
    public PdfPTable(PdfPTable table) {
        copyFormat(table);
        for (int k = 0; k < currentRow.length; ++k) {
            if (table.currentRow[k] == null)
                break;
            currentRow[k] = new PdfPCell(table.currentRow[k]);
        }
        for (int k = 0; k < table.rows.size(); ++k) {
            PdfPRow row = (PdfPRow)(table.rows.get(k));
            if (row != null)
                row = new PdfPRow(row);
            rows.add(row);
        }
    }
    
    
    public static PdfPTable shallowCopy(PdfPTable table) {
        PdfPTable nt = new PdfPTable();
        nt.copyFormat(table);
        return nt;
    }

    
    private void copyFormat(PdfPTable sourceTable) {
        relativeWidths = new float[sourceTable.relativeWidths.length];
        absoluteWidths = new float[sourceTable.relativeWidths.length];
        System.arraycopy(sourceTable.relativeWidths, 0, relativeWidths, 0, relativeWidths.length);
        System.arraycopy(sourceTable.absoluteWidths, 0, absoluteWidths, 0, relativeWidths.length);
        totalWidth = sourceTable.totalWidth;
        totalHeight = sourceTable.totalHeight;
        currentRowIdx = 0;
        tableEvent = sourceTable.tableEvent;
        runDirection = sourceTable.runDirection;
        defaultCell = new PdfPCell(sourceTable.defaultCell);
        currentRow = new PdfPCell[sourceTable.currentRow.length];
        isColspan = sourceTable.isColspan;
        splitRows = sourceTable.splitRows;
        spacingAfter = sourceTable.spacingAfter;
        spacingBefore = sourceTable.spacingBefore;
        headerRows = sourceTable.headerRows;
        footerRows = sourceTable.footerRows;
        lockedWidth = sourceTable.lockedWidth;
        extendLastRow = sourceTable.extendLastRow;
        headersInEvent = sourceTable.headersInEvent;
        widthPercentage = sourceTable.widthPercentage;
        splitLate = sourceTable.splitLate;
        skipFirstHeader = sourceTable.skipFirstHeader;
        horizontalAlignment = sourceTable.horizontalAlignment;
        keepTogether = sourceTable.keepTogether;
    }

        
    public void setWidths(float relativeWidths[]) throws DocumentException {
        if (relativeWidths.length != this.relativeWidths.length)
            throw new DocumentException("Wrong number of columns.");
        this.relativeWidths = new float[relativeWidths.length];
        System.arraycopy(relativeWidths, 0, this.relativeWidths, 0, relativeWidths.length);
        absoluteWidths = new float[relativeWidths.length];
        totalHeight = 0;
        calculateWidths();
        calculateHeights();
    }

        
    public void setWidths(int relativeWidths[]) throws DocumentException {
        float tb[] = new float[relativeWidths.length];
        for (int k = 0; k < relativeWidths.length; ++k)
            tb[k] = relativeWidths[k];
        setWidths(tb);
    }

    private void calculateWidths() {
        if (totalWidth <= 0)
            return;
        float total = 0;
        for (int k = 0; k < absoluteWidths.length; ++k) {
            total += relativeWidths[k];
        }
        for (int k = 0; k < absoluteWidths.length; ++k) {
            absoluteWidths[k] = totalWidth * relativeWidths[k] / total;
        }
    }
    
        
    public void setTotalWidth(float totalWidth) {
        if (this.totalWidth == totalWidth)
            return;
        this.totalWidth = totalWidth;
        totalHeight = 0;
        calculateWidths();
        calculateHeights();
    }

        
    public void setTotalWidth(float columnWidth[]) throws DocumentException {
        if (columnWidth.length != this.relativeWidths.length)
            throw new DocumentException("Wrong number of columns.");
        totalWidth = 0;
        for (int k = 0; k < columnWidth.length; ++k)
            totalWidth += columnWidth[k];
        setWidths(columnWidth);
    }

        
    public void setWidthPercentage(float columnWidth[], Rectangle pageSize) throws DocumentException {
        if (columnWidth.length != this.relativeWidths.length)
            throw new IllegalArgumentException("Wrong number of columns.");
        float totalWidth = 0;
        for (int k = 0; k < columnWidth.length; ++k)
            totalWidth += columnWidth[k];
        widthPercentage = totalWidth / (pageSize.getRight() - pageSize.getLeft()) * 100f;
        setWidths(columnWidth);
    }

        
    public float getTotalWidth() {
        return totalWidth;
    }

    void calculateHeights() {
        if (totalWidth <= 0)
            return;
        totalHeight = 0;
        for (int k = 0; k < rows.size(); ++k) {
            PdfPRow row = (PdfPRow)rows.get(k);
            if (row != null) {
                row.setWidths(absoluteWidths);
                totalHeight += row.getMaxHeights();
            }
        }
    }
    
    
    public void calculateHeightsFast() {
        if (totalWidth <= 0)
            return;
        totalHeight = 0;
        for (int k = 0; k < rows.size(); ++k) {
            PdfPRow row = (PdfPRow)rows.get(k);
            if (row != null)
                totalHeight += row.getMaxHeights();
        }
    }
    
        
    public PdfPCell getDefaultCell() {
        return defaultCell;
    }
    
        
    public void addCell(PdfPCell cell) {
        PdfPCell ncell = new PdfPCell(cell);
        int colspan = ncell.getColspan();
        colspan = Math.max(colspan, 1);
        colspan = Math.min(colspan, currentRow.length - currentRowIdx);
        ncell.setColspan(colspan);
        if (colspan != 1)
            isColspan = true;
        int rdir = ncell.getRunDirection();
        if (rdir == PdfWriter.RUN_DIRECTION_DEFAULT)
            ncell.setRunDirection(runDirection);
        currentRow[currentRowIdx] = ncell;
        currentRowIdx += colspan;
        if (currentRowIdx >= currentRow.length) {
            if (runDirection == PdfWriter.RUN_DIRECTION_RTL) {
                PdfPCell rtlRow[] = new PdfPCell[absoluteWidths.length];
                int rev = currentRow.length;
                for (int k = 0; k < currentRow.length; ++k) {
                    PdfPCell rcell = currentRow[k];
                    int cspan = rcell.getColspan();
                    rev -= cspan;
                    rtlRow[rev] = rcell;
                    k += cspan - 1;
                }
                currentRow = rtlRow;
            }
            PdfPRow row = new PdfPRow(currentRow);
            if (totalWidth > 0) {
                row.setWidths(absoluteWidths);
                totalHeight += row.getMaxHeights();
            }
            rows.add(row);
            currentRow = new PdfPCell[absoluteWidths.length];
            currentRowIdx = 0;
        }
    }
    
        
    public void addCell(String text) {
        addCell(new Phrase(text));
    }
    
        
    public void addCell(PdfPTable table) {
        defaultCell.setTable(table);
        addCell(defaultCell);
        defaultCell.setTable(null);
    }
    
        
    public void addCell(Image image) {
        defaultCell.setImage(image);
        addCell(defaultCell);
        defaultCell.setImage(null);
    }
    
        
    public void addCell(Phrase phrase) {
        defaultCell.setPhrase(phrase);
        addCell(defaultCell);
        defaultCell.setPhrase(null);
    }
    
        
    public float writeSelectedRows(int rowStart, int rowEnd, float xPos, float yPos, PdfContentByte[] canvases) {
        return writeSelectedRows(0, -1, rowStart, rowEnd, xPos, yPos, canvases);
    }
    
        
    public float writeSelectedRows(int colStart, int colEnd, int rowStart, int rowEnd, float xPos, float yPos, PdfContentByte[] canvases) {
        if (totalWidth <= 0)
            throw new RuntimeException("The table width must be greater than zero.");
        int size = rows.size();
        if (rowEnd < 0)
            rowEnd = size;
        rowEnd = Math.min(rowEnd, size);
        if (rowStart < 0)
            rowStart = 0;
        if (rowStart >= rowEnd)
            return yPos;
        if (colEnd < 0)
            colEnd = absoluteWidths.length;
        colEnd = Math.min(colEnd, absoluteWidths.length);
        if (colStart < 0)
            colStart = 0;
        colStart = Math.min(colStart, absoluteWidths.length);
        float yPosStart = yPos;
        for (int k = rowStart; k < rowEnd; ++k) {
            PdfPRow row = (PdfPRow)rows.get(k);
            if (row != null) {
                row.writeCells(colStart, colEnd, xPos, yPos, canvases);
                yPos -= row.getMaxHeights();
            }
        }
        if (tableEvent != null && colStart == 0 && colEnd == absoluteWidths.length) {
            float heights[] = new float[rowEnd - rowStart + 1];
            heights[0] = yPosStart;
            for (int k = rowStart; k < rowEnd; ++k) {
                PdfPRow row = (PdfPRow)rows.get(k);
                float hr = 0;
                if (row != null)
                    hr = row.getMaxHeights();
                heights[k - rowStart + 1] = heights[k - rowStart] - hr;
            }
            tableEvent.tableLayout(this, getEventWidths(xPos, rowStart, rowEnd, headersInEvent), heights, headersInEvent ? headerRows : 0, rowStart, canvases);
        }
        return yPos;
    }
    
        
    public float writeSelectedRows(int rowStart, int rowEnd, float xPos, float yPos, PdfContentByte canvas) {
        return writeSelectedRows(0, -1, rowStart, rowEnd, xPos, yPos, canvas);
    }
    
        
    public float writeSelectedRows(int colStart, int colEnd, int rowStart, int rowEnd, float xPos, float yPos, PdfContentByte canvas) {
        if (colEnd < 0)
            colEnd = absoluteWidths.length;
        colEnd = Math.min(colEnd, absoluteWidths.length);
        if (colStart < 0)
            colStart = 0;
        colStart = Math.min(colStart, absoluteWidths.length);
        if (colStart != 0 || colEnd != absoluteWidths.length) {
            float w = 0;
            for (int k = colStart; k < colEnd; ++k)
                w += absoluteWidths[k];
            canvas.saveState();
            float lx = 0;
            float rx = 0;
            if (colStart == 0)
                lx = 10000;
            if (colEnd == absoluteWidths.length)
                rx = 10000;
            canvas.rectangle(xPos - lx, -10000, w + lx + rx, 20000);
            canvas.clip();
            canvas.newPath();
        }
        PdfContentByte[] canvases = beginWritingRows(canvas);
        float y = writeSelectedRows(colStart, colEnd, rowStart, rowEnd, xPos, yPos, canvases);
        endWritingRows(canvases);
        if (colStart != 0 || colEnd != absoluteWidths.length)
            canvas.restoreState();
        return y;
    }
    
        
    public static PdfContentByte[] beginWritingRows(PdfContentByte canvas) {
        return new PdfContentByte[]{
            canvas,
            canvas.getDuplicate(),
            canvas.getDuplicate(),
            canvas.getDuplicate(),
        };
    }
    
        
    public static void endWritingRows(PdfContentByte[] canvases) {
        PdfContentByte canvas = canvases[BASECANVAS];
        canvas.saveState();
        canvas.add(canvases[BACKGROUNDCANVAS]);
        canvas.restoreState();
        canvas.saveState();
        canvas.setLineCap(2);
        canvas.resetRGBColorStroke();
        canvas.add(canvases[LINECANVAS]);
        canvas.restoreState();
        canvas.add(canvases[TEXTCANVAS]);
    }
    
        
    public int size() {
        return rows.size();
    }
    
        
    public float getTotalHeight() {
        return totalHeight;
    }
    
        
    public float getRowHeight(int idx) {
        if (totalWidth <= 0 || idx < 0 || idx >= rows.size())
            return 0;
        PdfPRow row = (PdfPRow)rows.get(idx);
        if (row == null)
            return 0;
        return row.getMaxHeights();
    }
    
        
    public float getHeaderHeight() {
        float total = 0;
        int size = Math.min(rows.size(), headerRows);
        for (int k = 0; k < size; ++k) {
            PdfPRow row = (PdfPRow)rows.get(k);
            if (row != null)
                total += row.getMaxHeights();
        }
        return total;
    }
    
        
    public boolean deleteRow(int rowNumber) {
        if (rowNumber < 0 || rowNumber >= rows.size()) {
            return false;
        }
        if (totalWidth > 0) {
            PdfPRow row = (PdfPRow)rows.get(rowNumber);
            if (row != null)
                totalHeight -= row.getMaxHeights();
        }
        rows.remove(rowNumber);
        return true;
    }
    
        
    public boolean deleteLastRow() {
        return deleteRow(rows.size() - 1);
    }
    
    
    public void deleteBodyRows() {
        ArrayList rows2 = new ArrayList();
        for (int k = 0; k < headerRows; ++k)
            rows2.add(rows.get(k));
        rows = rows2;
        totalHeight = 0;
        if (totalWidth > 0)
            totalHeight = getHeaderHeight();
    }

    
    public int getHeaderRows() {
        return headerRows;
    }
    
    
    public void setHeaderRows(int headerRows) {
        if (headerRows < 0)
            headerRows = 0;
        this.headerRows = headerRows;
    }
    
    
    public ArrayList getChunks() {
        return new ArrayList();
    }
    
    
    public int type() {
        return Element.PTABLE;
    }
    
    
    public boolean process(ElementListener listener) {
        try {
            return listener.add(this);
        }
        catch(DocumentException de) {
            return false;
        }
    }
    
    
    public float getWidthPercentage() {
        return widthPercentage;
    }
    
    
    public void setWidthPercentage(float widthPercentage) {
        this.widthPercentage = widthPercentage;
    }
    
    
    public int getHorizontalAlignment() {
        return horizontalAlignment;
    }
    
    
    public void setHorizontalAlignment(int horizontalAlignment) {
        this.horizontalAlignment = horizontalAlignment;
    }
    
    
    public PdfPRow getRow(int idx) {
        return (PdfPRow)rows.get(idx);
    }

    
    public ArrayList getRows() {
        return rows;
    }

        
    public void setTableEvent(PdfPTableEvent event) {
        if (event == null) this.tableEvent = null;
        else if (this.tableEvent == null) this.tableEvent = event;
        else if (this.tableEvent instanceof PdfPTableEventForwarder) ((PdfPTableEventForwarder)this.tableEvent).addTableEvent(event);
        else {
            PdfPTableEventForwarder forward = new PdfPTableEventForwarder();
            forward.addTableEvent(this.tableEvent);
            forward.addTableEvent(event);
            this.tableEvent = forward;
        }
    }
    
        
    public PdfPTableEvent getTableEvent() {
        return tableEvent;
    }
    
        
    public float[] getAbsoluteWidths() {
        return absoluteWidths;
    }
    
    float [][] getEventWidths(float xPos, int firstRow, int lastRow, boolean includeHeaders) {
        if (includeHeaders) {
            firstRow = Math.max(firstRow, headerRows);
            lastRow = Math.max(lastRow, headerRows);
        }
        float widths[][] = new float[(includeHeaders ? headerRows : 0) + lastRow - firstRow][];
        if (isColspan) {
            int n = 0;
            if (includeHeaders) {
                for (int k = 0; k < headerRows; ++k) {
                    PdfPRow row = (PdfPRow)rows.get(k);
                    if (row == null)
                        ++n;
                    else
                        widths[n++] = row.getEventWidth(xPos);
                }
            }
            for (; firstRow < lastRow; ++firstRow) {
                    PdfPRow row = (PdfPRow)rows.get(firstRow);
                    if (row == null)
                        ++n;
                    else
                        widths[n++] = row.getEventWidth(xPos);
            }
        }
        else {
            float width[] = new float[absoluteWidths.length + 1];
            width[0] = xPos;
            for (int k = 0; k < absoluteWidths.length; ++k)
                width[k + 1] = width[k] + absoluteWidths[k];
            for (int k = 0; k < widths.length; ++k)
                widths[k] = width;
        }
        return widths;
    }


    
    public boolean isSkipFirstHeader() {
        return skipFirstHeader;
    }
    
    
    public void setSkipFirstHeader(boolean skipFirstHeader) {
        this.skipFirstHeader = skipFirstHeader;
    }

    
    public void setRunDirection(int runDirection) {
        if (runDirection < PdfWriter.RUN_DIRECTION_DEFAULT || runDirection > PdfWriter.RUN_DIRECTION_RTL)
            throw new RuntimeException("Invalid run direction: " + runDirection);
        this.runDirection = runDirection;
    }
    
    
    public int getRunDirection() {
        return runDirection;
    }
    
    
    public boolean isLockedWidth() {
        return this.lockedWidth;
    }
    
    
    public void setLockedWidth(boolean lockedWidth) {
        this.lockedWidth = lockedWidth;
    }
    
    
    public boolean isSplitRows() {
        return this.splitRows;
    }
    
    
    public void setSplitRows(boolean splitRows) {
        this.splitRows = splitRows;
    }
    

    
    public void setSpacingBefore(float spacing) {
        this.spacingBefore = spacing;
    }
    

    
    public void setSpacingAfter(float spacing) {
        this.spacingAfter = spacing;
    }    


    
    public float spacingBefore() {
        return spacingBefore;
    }
    

    
    public float spacingAfter() {
        return spacingAfter;
    }    
    
    
    public boolean isExtendLastRow() {
        return this.extendLastRow;
    }
    
    
    public void setExtendLastRow(boolean extendLastRow) {
        this.extendLastRow = extendLastRow;
    }
    
    
    public boolean isHeadersInEvent() {
        return this.headersInEvent;
    }
    
    
    public void setHeadersInEvent(boolean headersInEvent) {
        this.headersInEvent = headersInEvent;
    }
    
    
    public boolean isSplitLate() {
        return this.splitLate;
    }
    
    
    public void setSplitLate(boolean splitLate) {
        this.splitLate = splitLate;
    }
    
    
    public void setKeepTogether(boolean p_KeepTogether) {
        keepTogether = p_KeepTogether;
    }
     
    public boolean getKeepTogether() {
        return keepTogether;
    }
    
    
    public int getFooterRows() {
        return this.footerRows;
    }
    
    
    public void setFooterRows(int footerRows) {
        if (footerRows < 0)
            footerRows = 0;
        this.footerRows = footerRows;
    }
    
    
    public void completeRow() {
        while (currentRowIdx > 0) {
            addCell(defaultCell);
        }
    }
}