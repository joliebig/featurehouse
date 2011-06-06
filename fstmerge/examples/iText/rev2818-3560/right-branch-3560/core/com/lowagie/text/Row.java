

package com.lowagie.text;

import java.util.ArrayList;


public class Row implements Element {
    
    
    
    
    public static final int NULL = 0;
    
    
    public static final int CELL = 1;
    
    
    public static final int TABLE = 2;
    
    
    
    
    protected int columns;
    
    
    protected int currentColumn;
    
    
    protected boolean[] reserved;
    
    
    protected Object[] cells;
    
    
    protected int horizontalAlignment;
    
    
    
    
    protected Row(int columns) {
        this.columns = columns;
        reserved = new boolean[columns];
        cells = new Object[columns];
        currentColumn = 0;
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
        return Element.ROW;
    }
    
    
    public ArrayList<Chunk> getChunks() {
        return new ArrayList<Chunk>();
    }
    
    
    public boolean isContent() {
        return true;
    }

    
    public boolean isNestable() {
        return false;
    }
    
    
    
        
    void deleteColumn(int column) {
        if ((column >= columns) || (column < 0)) {
            throw new IndexOutOfBoundsException("getCell at illegal index : " + column);
        }
        columns--;
        boolean newReserved[] = new boolean[columns];
        Object newCells[] = new Cell[columns];
        
        for (int i = 0; i < column; i++) {
            newReserved[i] = reserved[i];
            newCells[i] = cells[i];
            if (newCells[i] != null && (i + ((Cell) newCells[i]).getColspan() > column)) {
                ((Cell) newCells[i]).setColspan(((Cell) cells[i]).getColspan() - 1);
            }
        }
        for (int i = column; i < columns; i++) {
            newReserved[i] = reserved[i + 1];
            newCells[i] = cells[i + 1];
        }
        if (cells[column] != null && ((Cell) cells[column]).getColspan() > 1) {
            newCells[column] = cells[column];
            ((Cell) newCells[column]).setColspan(((Cell) newCells[column]).getColspan() - 1);
        }
        reserved = newReserved;
        cells = newCells;
    }
    
    
    
    
    int addElement(Object element) {
        return addElement(element, currentColumn);
    }
    
    
    int addElement(Object element, int column) {
        if (element == null) throw new NullPointerException("addCell - null argument");
        if ((column < 0) || (column > columns)) throw new IndexOutOfBoundsException("addCell - illegal column argument");
        if ( !((getObjectID(element) == CELL) || (getObjectID(element) == TABLE)) ) throw new IllegalArgumentException("addCell - only Cells or Tables allowed");
        
        int lColspan = ( (Cell.class.isInstance(element)) ? ((Cell) element).getColspan() : 1);
        
        if (!reserve(column, lColspan)) {
            return -1;
        }
        
        cells[column] = element;
        currentColumn += lColspan - 1;
        
        return column;
    }
    
    
    void setElement(Object aElement, int column) {
        if (reserved[column]) throw new IllegalArgumentException("setElement - position already taken");
        
        cells[column] = aElement;
        if (aElement != null) {
            reserved[column] = true;
        }
    }
    
    
    boolean reserve(int column) {
        return reserve(column, 1);
    }
    
    
    
    boolean reserve(int column, int size) {
        if ((column < 0) || ((column + size) > columns)) throw new IndexOutOfBoundsException("reserve - incorrect column/size");
        
        for(int i=column; i < column + size; i++)
        {
            if (reserved[i]) {
                
                for(int j=i; j >= column; j--) {
                    reserved[j] = false;
                }
                return false;
            }
            reserved[i] = true;
        }
        return true;
    }
    
    
    
    
    boolean isReserved(int column) {
        return reserved[column];
    }
    
    
    int getElementID(int column) {
        if (cells[column] == null) return NULL;
        else if (Cell.class.isInstance(cells[column])) return CELL;
        else if (Table.class.isInstance(cells[column])) return TABLE;
        
        return -1;
    }
    
    
    int getObjectID(Object element) {
        if (element == null) return NULL;
        else if (Cell.class.isInstance(element)) return CELL;
        else if (Table.class.isInstance(element)) return TABLE; 
        return -1;
    }
    
    
    public Object getCell(int column) {
        if ((column < 0) || (column > columns)) {
            throw new IndexOutOfBoundsException("getCell at illegal index :" + column + " max is " + columns);
        }
        return cells[column];
    }
    
    
    public boolean isEmpty() {
        for (int i = 0; i < columns; i++) {
            if (cells[i] != null) {
                return false;
            }
        }
        return true;
    }
    
    
    public int getColumns() {
        return columns;
    }
    
    
    public void setHorizontalAlignment(int value) {
        horizontalAlignment = value;
    }
    
    
    public int getHorizontalAlignment() {
        return horizontalAlignment;
    }
}
