

package com.lowagie.text;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;



public class Table extends Rectangle implements LargeElement {
    
    
    
    
    private int columns;
    
    
    private ArrayList rows = new ArrayList();
    
    
    private Point curPosition = new Point(0, 0);
    
    
    private Cell defaultCell = new Cell(true);
    
    
    private int lastHeaderRow = -1;
    
    
    private int alignment = Element.ALIGN_CENTER;
    
    
    private float cellpadding;
    
    
    private float cellspacing;
    
    
    private float width = 80;
    
    
    private boolean locked = false;
    
    
    private float[] widths;
    
    
    private boolean mTableInserted = false;
    
    
    protected boolean autoFillEmptyCells = false;
    
    
    boolean tableFitsPage = false;
    
    
    boolean cellsFitPage = false;
    
    
    float offset = Float.NaN;
    
    
    protected boolean convert2pdfptable = false;
    
    
    protected boolean notAddedYet = true;
    
    
    protected boolean complete = true;
    
    
    
    
    public Table(int columns) throws BadElementException {
        this(columns, 1);
    }
    
    
    public Table(int columns, int rows) throws BadElementException {
        
        super(0, 0, 0, 0);
        setBorder(BOX);
        setBorderWidth(1);
        defaultCell.setBorder(BOX);
        
        
        if (columns <= 0) {
            throw new BadElementException("A table should have at least 1 column.");
        }
        this.columns = columns;
        
        
        for (int i = 0; i < rows; i++) {
            this.rows.add(new Row(columns));
        }
        curPosition = new Point(0, 0);
        
        
        widths = new float[columns];
        float width = 100f / columns;
        for (int i = 0; i < columns; i++) {
            widths[i] = width;
        }
    }
    
    
    public Table(Table t) {
        super(0, 0, 0, 0);
        this.cloneNonPositionParameters(t);
        this.columns = t.columns;
        this.rows = t.rows;
        this.curPosition = t.curPosition;
        this.defaultCell = t.defaultCell;
        this.lastHeaderRow = t.lastHeaderRow;
        this.alignment = t.alignment;
        this.cellpadding = t.cellpadding;
        this.cellspacing = t.cellspacing;
        this.width = t.width;
        this.widths = t.widths;
        this.autoFillEmptyCells = t.autoFillEmptyCells;
        this.tableFitsPage = t.tableFitsPage;
        this.cellsFitPage = t.cellsFitPage;
        this.offset = t.offset;
        this.convert2pdfptable = t.convert2pdfptable;
    }
    
    
    
    
    public boolean process(ElementListener listener) {
        try {
            return listener.add(this);
        }
        catch(DocumentException de) {
            return false;
        }
    }
    
    
    public int type() {
        return Element.TABLE;
    }
    
    
    
    public ArrayList getChunks() {
        return new ArrayList();
    }

    
    public boolean isNestable() {
        return true;
    }
    
    

    
    public int getColumns() {
        return columns;
    }
    
    
    public int size() {
        return rows.size();
    }
    
    
    public Dimension getDimension() {
        return new Dimension(columns, size());
    }
    
    
    public Cell getDefaultCell() {
        return defaultCell;
    }
    
    
    public void setDefaultCell(Cell value) {
        defaultCell = value;
    }

    
    public int getLastHeaderRow() {
        return this.lastHeaderRow;
    }
    
    
    public void setLastHeaderRow(int value) {
        lastHeaderRow = value;
    }
    
    
    public int endHeaders() {
        lastHeaderRow = curPosition.x - 1;
        return lastHeaderRow;
    }

    
    public int getAlignment() {
        return alignment;
    }
    
    
    public void setAlignment(int value) {
        alignment = value;
    }
    
    
    public void setAlignment(String alignment) {
        if (ElementTags.ALIGN_LEFT.equalsIgnoreCase(alignment)) {
            this.alignment = Element.ALIGN_LEFT;
            return;
        }
        if (ElementTags.RIGHT.equalsIgnoreCase(alignment)) {
            this.alignment = Element.ALIGN_RIGHT;
            return;
        }
        this.alignment = Element.ALIGN_CENTER;
    }

    
    public float getPadding() {
        return cellpadding;
    }
    
    
    public void setPadding(float value) {
        cellpadding = value;
    }

    
    public float getSpacing() {
        return cellspacing;
    }
    
    
    public void setSpacing(float value) {
        cellspacing = value;
    }
    
    
    public void setAutoFillEmptyCells(boolean aDoAutoFill) {
        autoFillEmptyCells = aDoAutoFill;
    }

    
    public float getWidth() {
        return width;
    }
    
    
    public void setWidth(float width) {
        this.width = width;
    }
    
    
    public boolean isLocked() {
        return locked;
    }

    
    public void setLocked(boolean locked) {
        this.locked = locked;
    }

    
    public float[] getProportionalWidths() {
        return widths;
    }
    
    
    public void setWidths(float[] widths) throws BadElementException {
        if (widths.length != columns) {
            throw new BadElementException("Wrong number of columns.");
        }
        
        
        float hundredPercent = 0;
        for (int i = 0; i < columns; i++) {
            hundredPercent += widths[i];
        }
        
        
        float width;
        this.widths[columns - 1] = 100;
        for (int i = 0; i < columns - 1; i++) {
            width = (100.0f * widths[i]) / hundredPercent;
            this.widths[i] = width;
            this.widths[columns - 1] -= width;
        }
    }
    
    
    public void setWidths(int[] widths) throws DocumentException {
        float tb[] = new float[widths.length];
        for (int k = 0; k < widths.length; ++k)
            tb[k] = widths[k];
        setWidths(tb);
    }
    
    
    public boolean isTableFitsPage() {
        return tableFitsPage;
    }
    
    
    public void setTableFitsPage(boolean fitPage) {
        this.tableFitsPage = fitPage;
        if (fitPage) setCellsFitPage(true);
    }
    
    
    public boolean isCellsFitPage() {
        return cellsFitPage;
    }
    
    
    public void setCellsFitPage(boolean fitPage) {
        this.cellsFitPage = fitPage;
    }
    
    
    public void setOffset(float offset) {
        this.offset = offset;
    }
    
    
    public float getOffset() {
        return offset;
    }
    
    
    public boolean isConvert2pdfptable() {
        return convert2pdfptable;
    }
    
    public void setConvert2pdfptable(boolean convert2pdfptable) {
        this.convert2pdfptable = convert2pdfptable;
    }
    
    
    
    
    public void addCell(Cell aCell, int row, int column) throws BadElementException {
        addCell(aCell, new Point(row,column));
    }
    
    
    public void addCell(Cell aCell, Point aLocation) throws BadElementException {
        if (aCell == null) throw new NullPointerException("addCell - cell has null-value");
        if (aLocation == null) throw new NullPointerException("addCell - point has null-value");
        if (aCell.isTable()) insertTable((Table)aCell.getElements().next(), aLocation);
        
        if (aLocation.x < 0) throw new BadElementException("row coordinate of location must be >= 0");
        if ((aLocation.y <= 0) && (aLocation.y > columns)) throw new BadElementException("column coordinate of location must be >= 0 and < nr of columns");
        if (!isValidLocation(aCell, aLocation)) throw new BadElementException("Adding a cell at the location (" + aLocation.x + "," + aLocation.y + ") with a colspan of " + aCell.getColspan() + " and a rowspan of " + aCell.getRowspan() + " is illegal (beyond boundaries/overlapping).");
        
        if (aCell.getBorder() == UNDEFINED) aCell.setBorder(defaultCell.getBorder());
        aCell.fill();
        placeCell(rows, aCell, aLocation);
        setCurrentLocationToNextValidPosition(aLocation);
    }
    
    
    public void addCell(Cell cell) {
        try {
            addCell(cell, curPosition);
        }
        catch(BadElementException bee) {
            
        }
    }
    
    
    public void addCell(Phrase content) throws BadElementException {
        addCell(content, curPosition);
    }
    
    
    public void addCell(Phrase content, Point location) throws BadElementException {
        Cell cell = new Cell(content);
        cell.setBorder(defaultCell.getBorder());
        cell.setBorderWidth(defaultCell.getBorderWidth());
        cell.setBorderColor(defaultCell.getBorderColor());
        cell.setBackgroundColor(defaultCell.getBackgroundColor());
        cell.setHorizontalAlignment(defaultCell.getHorizontalAlignment());
        cell.setVerticalAlignment(defaultCell.getVerticalAlignment());
        cell.setColspan(defaultCell.getColspan());
        cell.setRowspan(defaultCell.getRowspan());
        addCell(cell, location);
    }
    
    
    
    public void addCell(String content) throws BadElementException {
        addCell(new Phrase(content), curPosition);
    }
    
    
    public void addCell(String content, Point location) throws BadElementException {
        addCell(new Phrase(content), location);
    }
    
    
    public void insertTable(Table aTable) {
        if (aTable == null) throw new NullPointerException("insertTable - table has null-value");
        insertTable(aTable, curPosition);
    }
    
    
    public void insertTable(Table aTable, int row, int column) {
        if (aTable == null) throw new NullPointerException("insertTable - table has null-value");
        insertTable(aTable, new Point(row, column));
    }
    
    
    public void insertTable(Table aTable, Point aLocation) {
        
        if (aTable == null) throw new NullPointerException("insertTable - table has null-value");
        if (aLocation == null) throw new NullPointerException("insertTable - point has null-value");
        mTableInserted = true;
        aTable.complete();
        
        if (aLocation.y > columns) {
            throw new IllegalArgumentException("insertTable -- wrong columnposition("+ aLocation.y + ") of location; max =" + columns);
        }
        
        int rowCount = aLocation.x + 1 - rows.size();
        int i = 0;
        if ( rowCount > 0 ) {   
            for (; i < rowCount; i++) {
                rows.add(new Row(columns));
            }
        }
        
        ((Row) rows.get(aLocation.x)).setElement(aTable,aLocation.y);
        
        setCurrentLocationToNextValidPosition(aLocation);
    }
    
    
    public void addColumns(int aColumns) {
        ArrayList newRows = new ArrayList(rows.size());
        
        int newColumns = columns + aColumns;
        Row row;
        for (int i = 0; i < rows.size(); i++) {
            row = new Row(newColumns);
            for (int j = 0; j < columns; j++) {
                row.setElement(((Row) rows.get(i)).getCell(j) ,j);
            }
            for (int j = columns; j < newColumns && i < curPosition.x; j++) {
                row.setElement(null, j);
            }
            newRows.add(row);
        }
        
        float [] newWidths = new float[newColumns];
        System.arraycopy(widths, 0, newWidths, 0, columns);
        for (int j = columns; j < newColumns ; j++) {
            newWidths[j] = 0;
        }
        columns = newColumns;
        widths = newWidths;
        rows = newRows;
    }
    
    
    public void deleteColumn(int column) throws BadElementException {
        float newWidths[] = new float[--columns];
        System.arraycopy(widths, 0, newWidths, 0, column);
        System.arraycopy(widths, column + 1, newWidths, column, columns - column);
        setWidths(newWidths);
        System.arraycopy(widths, 0, newWidths, 0, columns);
        widths = newWidths;
        Row row;
        int size = rows.size();
        for (int i = 0; i < size; i++) {
            row = (Row) rows.get(i);
            row.deleteColumn(column);
            rows.set(i, row);
        }
        if (column == columns) {
            curPosition.setLocation(curPosition.x+1, 0);
        }
    }

    
    public boolean deleteRow(int row) {
        if (row < 0 || row >= rows.size()) {
            return false;
        }
        rows.remove(row);
        curPosition.setLocation(curPosition.x-1, curPosition.y);
        return true;
    }
    
    
    public void deleteAllRows() {
        rows.clear();
        rows.add(new Row(columns));
        curPosition.setLocation(0, 0);
        lastHeaderRow = -1;
    }
    
    
    public boolean deleteLastRow() {
        return deleteRow(rows.size() - 1);
    }
    
    
    public void complete() {
        if (mTableInserted) {
            mergeInsertedTables();  
            mTableInserted = false;
        }
        if (autoFillEmptyCells) {
            fillEmptyMatrixCells();
        }
    }
    
    
    
    
    private Object getElement(int row, int column) {
        return ((Row) rows.get(row)).getCell(column);
    }
    
    
    private void mergeInsertedTables() {
        int i=0, j=0;
        float [] lNewWidths = null;
        int [] lDummyWidths = new int[columns];     
        float[][] lDummyColumnWidths = new float[columns][]; 
        int [] lDummyHeights = new int[rows.size()]; 
        ArrayList newRows = null;
        boolean isTable=false;
        int lTotalRows  = 0, lTotalColumns      = 0;
        int lNewMaxRows = 0, lNewMaxColumns     = 0;
        
        Table lDummyTable = null;
        
        
        
        
        for (j=0; j < columns; j++) {
            lNewMaxColumns = 1; 
            float [] tmpWidths = null;
            for (i=0; i < rows.size(); i++) {
                if ( Table.class.isInstance(((Row) rows.get(i)).getCell(j)) ) {
                    isTable=true;
                    lDummyTable = ((Table) ((Row) rows.get(i)).getCell(j));
                    if( tmpWidths == null) {
                        tmpWidths = lDummyTable.widths;
                        lNewMaxColumns=tmpWidths.length;
                    }
                    else {
                        int cols = lDummyTable.getDimension().width;
                        float [] tmpWidthsN = new float[ cols * tmpWidths.length];
                        float tpW=0, btW=0, totW=0;
                        int tpI=0, btI=0, totI=0;
                        tpW+=tmpWidths[0];
                        btW+=lDummyTable.widths[0];
                        while( tpI<tmpWidths.length && btI<cols) {
                            if( btW>tpW) {
                                tmpWidthsN[totI] = tpW-totW;
                                tpI++;
                                if(tpI<tmpWidths.length) {
                                    tpW+=tmpWidths[tpI];
                                }
                            }
                            else {
                                tmpWidthsN[totI] = btW-totW;
                                btI++;
                                if(Math.abs(btW - tpW) < 0.0001) {
                                    tpI++;
                                    if(tpI<tmpWidths.length) {
                                        tpW+=tmpWidths[tpI];
                                    }
                                }
                                if(btI<cols) {
                                    btW+=lDummyTable.widths[btI];
                                }
                            }
                            totW+=tmpWidthsN[totI];
                            totI++;
                        }
                       
                        tmpWidths = new float[totI];
                        System.arraycopy(tmpWidthsN, 0, tmpWidths, 0, totI);
                        lNewMaxColumns=totI;
                    }
                                     
                }
            }
            lDummyColumnWidths[j] = tmpWidths;
            lTotalColumns += lNewMaxColumns;
            lDummyWidths [j] = lNewMaxColumns;
        }
        
        
        for (i=0; i < rows.size(); i++) {
            lNewMaxRows = 1;    
            for (j=0; j < columns; j++) {
                if ( Table.class.isInstance(((Row) rows.get(i)).getCell(j)) ) {
                    isTable=true;
                    lDummyTable = (Table) ((Row) rows.get(i)).getCell(j);
                    if ( lDummyTable.getDimension().height > lNewMaxRows ) {
                        lNewMaxRows = lDummyTable.getDimension().height;
                    }
                }
            }
            lTotalRows += lNewMaxRows;
            lDummyHeights [i] = lNewMaxRows;
        }
        
        if ( (lTotalColumns != columns) || (lTotalRows != rows.size()) || isTable)    
        {
            
            
            
            
            lNewWidths = new float [lTotalColumns];
            int lDummy = 0;
            for (int tel=0; tel < widths.length;tel++) {
                if ( lDummyWidths[tel] != 1) {
                    
                    for (int tel2 = 0; tel2 < lDummyWidths[tel]; tel2++) {
                        
                        lNewWidths[lDummy] = widths[tel] * lDummyColumnWidths[tel][tel2] / 100f; 
                        lDummy++;
                    }
                }
                else {
                    lNewWidths[lDummy] = widths[tel];
                    lDummy++;
                }
            }
            
            
            
            
            
            newRows = new ArrayList(lTotalRows);
            for (i = 0; i < lTotalRows; i++) {
                newRows.add(new Row(lTotalColumns));
            }
            int lDummyRow = 0, lDummyColumn = 0;        
            Object lDummyElement = null;
            for (i=0; i < rows.size(); i++) {
                lDummyColumn = 0;
                lNewMaxRows = 1;
                for (j=0; j < columns; j++) {
                    if ( Table.class.isInstance(((Row) rows.get(i)).getCell(j)) )       
                    {
                        lDummyTable = (Table) ((Row) rows.get(i)).getCell(j);
                        
                        
                        int colMap[] = new int[lDummyTable.widths.length+1];
                        int cb=0, ct=0;
                        
                        for( ; cb<lDummyTable.widths.length;cb++) {
                            colMap[cb] = lDummyColumn+ct;
                            
                            float wb;
                            wb = lDummyTable.widths[cb];
                            
                            float wt=0;
                            while( ct<lDummyWidths[j]) {
                                wt+=lDummyColumnWidths[j][ct++];
                                if(Math.abs(wb - wt) < 0.0001) break;
                            }
                        }
                        colMap[cb] = lDummyColumn+ct;
                        
                        
                        for (int k=0; k < lDummyTable.getDimension().height; k++) {
                            for (int l=0; l < lDummyTable.getDimension().width; l++) {
                                lDummyElement = lDummyTable.getElement(k,l);
                                if (lDummyElement != null) {
                                    int col=lDummyColumn+l;
                                    
                                    if ( Cell.class.isInstance(lDummyElement) ) {
                                        Cell lDummyC = (Cell)lDummyElement;
                                        
                                        col = colMap[l];
                                        int ot = colMap[l+lDummyC.getColspan()];
                                        
                                        lDummyC.setColspan(ot-col);
                                    }
                                    
                                    ((Row) newRows.get(k + lDummyRow)).addElement(lDummyElement,col);  
                                }
                            }
                        }
                    }
                    else        
                    {
                        Object aElement = getElement(i,j);
                        
                        if ( Cell.class.isInstance(aElement) ) {
                            
                            
                            ((Cell) aElement).setRowspan(((Cell) ((Row) rows.get(i)).getCell(j)).getRowspan() + lDummyHeights[i] - 1);
                            ((Cell) aElement).setColspan(((Cell) ((Row) rows.get(i)).getCell(j)).getColspan() + lDummyWidths[j] - 1);
                            
                            
                            placeCell(newRows,((Cell) aElement), new Point(lDummyRow,lDummyColumn));
                        }
                    }
                    lDummyColumn += lDummyWidths[j];
                }
                lDummyRow += lDummyHeights[i];
            }
            
            
            columns     = lTotalColumns;
            rows = newRows;
            this.widths = lNewWidths;
        }
    }
    
    
    private void fillEmptyMatrixCells() {
        try {
            for (int i=0; i < rows.size(); i++) {
                for (int j=0; j < columns; j++) {
                    if (!((Row) rows.get(i)).isReserved(j)) {
                        addCell(defaultCell, new Point(i, j));
                    }
                }
            }
        }
        catch(BadElementException bee) {
            throw new ExceptionConverter(bee);
        }
    }
    
    
    private boolean isValidLocation(Cell aCell, Point aLocation) {
        
        if ( aLocation.x < rows.size() )        
        {
            if ((aLocation.y + aCell.getColspan()) > columns) {
                return false;
            }
            
            int difx = ((rows.size() - aLocation.x) >  aCell.getRowspan()) ? aCell.getRowspan() : rows.size() - aLocation.x;
            int dify = ((columns - aLocation.y) >  aCell.getColspan()) ? aCell.getColspan() : columns - aLocation.y;
            
            for (int i=aLocation.x; i < (aLocation.x + difx); i++) {
                for (int j=aLocation.y; j < (aLocation.y + dify); j++) {
                    if (((Row) rows.get(i)).isReserved(j)) {
                        return false;
                    }
                }
            }
        }
        else {
            if ((aLocation.y + aCell.getColspan()) > columns) {
                return false;
            }
        }
        
        return true;
    }
    
    
    private void assumeTableDefaults(Cell aCell) {
        
        if (aCell.getBorder() == Rectangle.UNDEFINED) {
            aCell.setBorder(defaultCell.getBorder());
        }
        if (aCell.getBorderWidth() == Rectangle.UNDEFINED) {
            aCell.setBorderWidth(defaultCell.getBorderWidth());
        }
        if (aCell.getBorderColor() == null) {
            aCell.setBorderColor(defaultCell.getBorderColor());
        }
        if (aCell.getBackgroundColor() == null) {
            aCell.setBackgroundColor(defaultCell.getBackgroundColor());
        }
        if (aCell.getHorizontalAlignment() == Element.ALIGN_UNDEFINED) {
            aCell.setHorizontalAlignment(defaultCell.getHorizontalAlignment());
        }
        if (aCell.getVerticalAlignment() == Element.ALIGN_UNDEFINED) {
            aCell.setVerticalAlignment(defaultCell.getVerticalAlignment());
        }
    }
    
    
    private void placeCell(ArrayList someRows, Cell aCell, Point aPosition) {
        int i;
        Row row = null;
        int rowCount = aPosition.x + aCell.getRowspan() - someRows.size();
        assumeTableDefaults(aCell);
        if ( (aPosition.x + aCell.getRowspan()) > someRows.size() ) {
            for (i = 0; i < rowCount; i++) {
                row = new Row(columns);
                someRows.add(row);
            }
        }
        
        
        for (i = aPosition.x + 1; i < (aPosition.x  + aCell.getRowspan()); i++) {
            if ( !((Row) someRows.get(i)).reserve(aPosition.y, aCell.getColspan())) {
                
                
                throw new RuntimeException("addCell - error in reserve");
            }
        }
        row = (Row) someRows.get(aPosition.x);
        row.addElement(aCell, aPosition.y);
        
    }
    
    
    private void setCurrentLocationToNextValidPosition(Point aLocation)    {
        
        int i, j;
        i = aLocation.x;
        j = aLocation.y;
        do {
            if ( (j + 1)  == columns ) {    
                i++;
                j = 0;
            }
            else {
                j++;
            }
        }
        while (
        (i < rows.size()) && (j < columns) && (((Row) rows.get(i)).isReserved(j))
        );
        curPosition = new Point(i, j);
    }
    
    
    
    
    public float[] getWidths(float left, float totalWidth) {
        
        float[] w = new float[columns + 1];
        float wPercentage;
        if (locked) {
            wPercentage = 100 * width / totalWidth;
        }
        else {
            wPercentage = width;
        }
        
        switch(alignment) {
            case Element.ALIGN_LEFT:
                w[0] = left;
                break;
            case Element.ALIGN_RIGHT:
                w[0] = left + (totalWidth * (100 - wPercentage)) / 100;
                break;
            case Element.ALIGN_CENTER:
            default:
                w[0] = left + (totalWidth * (100 - wPercentage)) / 200;
        }
        
        totalWidth = (totalWidth * wPercentage) / 100;
        
        for (int i = 1; i < columns; i++) {
            w[i] = w[i - 1] + (widths[i - 1] * totalWidth / 100);
        }
        
        w[columns] = w[0] + totalWidth;
        return w;
    }
    
    
    public Iterator iterator() {
        return rows.iterator();
    }

    
    public PdfPTable createPdfPTable() throws BadElementException {
        if (!convert2pdfptable) {
            throw new BadElementException("No error, just an old style table");
        }
        setAutoFillEmptyCells(true);
        complete();
        PdfPTable pdfptable = new PdfPTable(widths);
        pdfptable.setComplete(complete);
        if (isNotAddedYet())
            pdfptable.setSkipFirstHeader(true);
        pdfptable.setTableEvent(SimpleTable.getDimensionlessInstance(this, cellspacing));
        pdfptable.setHeaderRows(lastHeaderRow + 1);
        pdfptable.setSplitLate(cellsFitPage);
        pdfptable.setKeepTogether(tableFitsPage);
        if (!Float.isNaN(offset)) {
            pdfptable.setSpacingBefore(offset);
        }
        pdfptable.setHorizontalAlignment(alignment);
        if (locked) {
            pdfptable.setTotalWidth(width);
            pdfptable.setLockedWidth(true);
        }
        else {
            pdfptable.setWidthPercentage(width);
        }
        Row row;
        for (Iterator iterator = iterator(); iterator.hasNext(); ) {
            row = (Row) iterator.next();
            Element cell;
            PdfPCell pcell;
            for (int i = 0; i < row.getColumns(); i++) {
                if ((cell = (Element)row.getCell(i)) != null) {
                    if (cell instanceof Table) {
                        pcell = new PdfPCell(((Table)cell).createPdfPTable());
                    }
                    else if (cell instanceof Cell) {
                        pcell = ((Cell)cell).createPdfPCell();
                         pcell.setPadding(cellpadding + cellspacing / 2f);
                         pcell.setCellEvent(SimpleCell.getDimensionlessInstance((Cell)cell, cellspacing));
                    }
                    else {
                        pcell = new PdfPCell();
                    }
                    pdfptable.addCell(pcell);
                }
            }
        }
        return pdfptable;
    }

    
    public boolean isNotAddedYet() {
        return notAddedYet;
    }

    
    public void setNotAddedYet(boolean notAddedYet) {
        this.notAddedYet = notAddedYet;
    }
    
    
    public void flushContent() {        
        this.setNotAddedYet(false);
        ArrayList headerrows = new ArrayList();
        for (int i = 0; i < getLastHeaderRow() + 1; i++) {
            headerrows.add(rows.get(i));
        }
        rows = headerrows;
    }

    
    public boolean isComplete() {
        return complete;
    }

    
    public void setComplete(boolean complete) {
        this.complete = complete;
    }
    
    
    
    
    public Table(java.util.Properties attributes) {
        this(com.lowagie.text.factories.ElementFactory.getTable(attributes));
    } 
    
    
    public int columns() {
        return getColumns();
    }
    
    
    public int alignment() {
        return getAlignment();
    }
    
    
    public float cellpadding() {
        return getPadding();
    }
    
    
    public float cellspacing() {
        return getSpacing();
    }
    
    
    public void setSpaceInsideCell(float value) {
        cellpadding = value;
    }
    
    
    public void setSpaceBetweenCells(float value) {
        cellspacing = value;
    }
    
    
    public int lastHeaderRow() {
        return getLastHeaderRow();
    }
    
    
    public float widthPercentage() {
        return getWidth();
    }
    
    
    public void setAbsWidth(String width) {
        setWidth(Float.parseFloat(width + "f"));
        setLocked(true);
    }
    
    
    public String absWidth() {
        if (isLocked())
            return String.valueOf(width);
        else
            return "";
    }
    
    
    
    
    public void setDefaultCellBorder(int value) {
        defaultCell.setBorder(value);
    }
    
    
    public void setDefaultCellBorderWidth(float value) {
        defaultCell.setBorderWidth(value);
    }
    
    
    public void setDefaultCellBorderColor(Color color) {
        defaultCell.setBorderColor(color);
    }
    
    
    public void setDefaultCellBackgroundColor(Color color) {
        defaultCell.setBackgroundColor(color);
    }
    
    
    public void setDefaultCellGrayFill(float value) {
        if (value >= 0 && value <= 1) {
            defaultCell.setGrayFill(value);
        }
    }
    
    
    public void setDefaultHorizontalAlignment(int value) {
        defaultCell.setHorizontalAlignment(value);
    }
    
    
    public void setDefaultVerticalAlignment(int value) {
        defaultCell.setVerticalAlignment(value);
    }
    
    
    public void setDefaultRowspan(int value) {
        defaultCell.setRowspan(value);
    }
    
    
    public void setDefaultColspan(int value) {
        defaultCell.setColspan(value);
    }
    
    
    public Cell getDefaultLayout() {
        return getDefaultCell();
    }
    
    
    public void setDefaultLayout(Cell value) {
        defaultCell = value;
    }
}
