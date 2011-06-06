

package com.lowagie.text.rtf;

import java.awt.Color;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Iterator;

import com.lowagie.text.Cell;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Rectangle;


public class RtfCell {
    

    
    private static final int MERGE_HORIZ_FIRST = 1;
    
    private static final int MERGE_VERT_FIRST = 2;
    
    private static final int MERGE_BOTH_FIRST = 3;
    
    private static final int MERGE_HORIZ_PREV = 4;
    
    private static final int MERGE_VERT_PREV = 5;
    
    private static final int MERGE_BOTH_PREV = 6;

    

    
    private static final byte[] cellMergeFirst = "clmgf".getBytes();
    
    private static final byte[] cellVMergeFirst = "clvmgf".getBytes();
    
    private static final byte[] cellMergePrev = "clmrg".getBytes();
    
    private static final byte[] cellVMergePrev = "clvmrg".getBytes();
    
    private static final byte[] cellVerticalAlignBottom = "clvertalb".getBytes();
    
    private static final byte[] cellVerticalAlignCenter = "clvertalc".getBytes();
    
    private static final byte[] cellVerticalAlignTop = "clvertalt".getBytes();
    
    private static final byte[] cellBorderLeft = "clbrdrl".getBytes();
    
    private static final byte[] cellBorderRight = "clbrdrr".getBytes();
    
    private static final byte[] cellBorderTop = "clbrdrt".getBytes();
    
    private static final byte[] cellBorderBottom = "clbrdrb".getBytes();
    
    private static final byte[] cellBackgroundColor = "clcbpat".getBytes();
    
    private static final byte[] cellWidthStyle = "clftsWidth3".getBytes();
    
    private static final byte[] cellWidthTag = "clwWidth".getBytes();
    
    private static final byte[] cellRightBorder = "cellx".getBytes();
    
    protected static final byte[] cellInTable = "intbl".getBytes();
    
    private static final byte[] cellEnd = "cell".getBytes();

    
    private static final byte[] cellPaddingTop = "clpadt".getBytes();
    
    private static final byte[] cellPaddingTopUnit = "clpadft3".getBytes();
    
    private static final byte[] cellPaddingBottom = "clpadb".getBytes();
    
    private static final byte[] cellPaddingBottomUnit = "clpadfb3".getBytes();
    
    private static final byte[] cellPaddingLeft = "clpadl".getBytes();
    
    private static final byte[] cellPaddingLeftUnit = "clpadfl3".getBytes();
    
    private static final byte[] cellPaddingRight = "clpadr".getBytes();
    
    private static final byte[] cellPaddingRightUnit = "clpadfr3".getBytes();

    
    private RtfWriter writer = null;
    
    private RtfTable mainTable = null;

    
    private int cellWidth = 0;
    
    private int cellRight = 0;
    
    private Cell store = null;
    
    private boolean emptyCell = true;
    
    private int mergeType = 0;
    
    private int cellpadding = 0;

    
    public RtfCell(RtfWriter writer, RtfTable mainTable) {
        super();
        this.writer = writer;
        this.mainTable = mainTable;
    }

    
    public int importCell(Cell cell, int cellLeft, int cellWidth, int x, int y, int cellpadding) {
        this.cellpadding = cellpadding;

        
        this.cellWidth = cellWidth;
        if (cell == null) {
            cellRight = cellLeft + cellWidth;
            return cellRight;
        }
        if (cell.getWidth() != 0) {
            this.cellWidth = (int) (cell.getWidth() * RtfWriter.TWIPSFACTOR);
        }
        cellRight = cellLeft + this.cellWidth;
        store = cell;
        emptyCell = false;
        if (cell.getColspan() > 1) {
            if (cell.getRowspan() > 1) {
                mergeType = MERGE_BOTH_FIRST;
                for (int i = y; i < y + cell.getRowspan(); i++) {
                    if (i > y) mainTable.setMerge(x, i, MERGE_VERT_PREV, this);
                    for (int j = x + 1; j < x + cell.getColspan(); j++) {
                        mainTable.setMerge(j, i, MERGE_BOTH_PREV, this);
                    }
                }
            } else {
                mergeType = MERGE_HORIZ_FIRST;
                for (int i = x + 1; i < x + cell.getColspan(); i++) {
                    mainTable.setMerge(i, y, MERGE_HORIZ_PREV, this);
                }
            }
        } else if (cell.getRowspan() > 1) {
            mergeType = MERGE_VERT_FIRST;
            for (int i = y + 1; i < y + cell.getRowspan(); i++) {
                mainTable.setMerge(x, i, MERGE_VERT_PREV, this);
            }
        }
        return cellRight;
    }

    
    public boolean writeCellSettings(ByteArrayOutputStream os) {
        try {
            float lWidth, tWidth, rWidth, bWidth;
            byte[] lStyle, tStyle, rStyle, bStyle;

            if (store instanceof RtfTableCell) {
                RtfTableCell c = (RtfTableCell) store;
                lWidth = c.leftBorderWidth();
                tWidth = c.topBorderWidth();
                rWidth = c.rightBorderWidth();
                bWidth = c.bottomBorderWidth();
                lStyle = RtfTableCell.getStyleControlWord(c.leftBorderStyle());
                tStyle = RtfTableCell.getStyleControlWord(c.topBorderStyle());
                rStyle = RtfTableCell.getStyleControlWord(c.rightBorderStyle());
                bStyle = RtfTableCell.getStyleControlWord(c.bottomBorderStyle());
            } else {
                lWidth = tWidth = rWidth = bWidth = store.getBorderWidth();
                lStyle = tStyle = rStyle = bStyle = RtfRow.tableBorder;
            }

            if (mergeType == MERGE_HORIZ_PREV || mergeType == MERGE_BOTH_PREV) {
                return true;
            }
            switch (mergeType) {
                case MERGE_VERT_FIRST:
                    os.write(RtfWriter.escape);
                    os.write(cellVMergeFirst);
                    break;
                case MERGE_BOTH_FIRST:
                    os.write(RtfWriter.escape);
                    os.write(cellVMergeFirst);
                    break;
                case MERGE_HORIZ_PREV:
                    os.write(RtfWriter.escape);
                    os.write(cellMergePrev);
                    break;
                case MERGE_VERT_PREV:
                    os.write(RtfWriter.escape);
                    os.write(cellVMergePrev);
                    break;
                case MERGE_BOTH_PREV:
                    os.write(RtfWriter.escape);
                    os.write(cellMergeFirst);
                    break;
            }
            switch (store.getVerticalAlignment()) {
                case Element.ALIGN_BOTTOM:
                    os.write(RtfWriter.escape);
                    os.write(cellVerticalAlignBottom);
                    break;
                case Element.ALIGN_CENTER:
                case Element.ALIGN_MIDDLE:
                    os.write(RtfWriter.escape);
                    os.write(cellVerticalAlignCenter);
                    break;
                case Element.ALIGN_TOP:
                    os.write(RtfWriter.escape);
                    os.write(cellVerticalAlignTop);
                    break;
            }

            if (((store.getBorder() & Rectangle.LEFT) == Rectangle.LEFT) &&
                    (lWidth > 0)) {
                os.write(RtfWriter.escape);
                os.write(cellBorderLeft);
                os.write(RtfWriter.escape);
                os.write(lStyle);
                os.write(RtfWriter.escape);
                os.write(RtfRow.tableBorderWidth);
                writeInt(os, (int) (lWidth * RtfWriter.TWIPSFACTOR));
                os.write(RtfWriter.escape);
                os.write(RtfRow.tableBorderColor);
                if (store.getBorderColor() == null)
                    writeInt(os, writer.addColor(new
                            Color(0, 0, 0)));
                else
                    writeInt(os, writer.addColor(store.getBorderColor()));
                os.write((byte) '\n');
            }
            if (((store.getBorder() & Rectangle.TOP) == Rectangle.TOP) && (tWidth > 0)) {
                os.write(RtfWriter.escape);
                os.write(cellBorderTop);
                os.write(RtfWriter.escape);
                os.write(tStyle);
                os.write(RtfWriter.escape);
                os.write(RtfRow.tableBorderWidth);
                writeInt(os, (int) (tWidth * RtfWriter.TWIPSFACTOR));
                os.write(RtfWriter.escape);
                os.write(RtfRow.tableBorderColor);
                if (store.getBorderColor() == null)
                    writeInt(os, writer.addColor(new
                            Color(0, 0, 0)));
                else
                    writeInt(os, writer.addColor(store.getBorderColor()));
                os.write((byte) '\n');
            }
            if (((store.getBorder() & Rectangle.BOTTOM) == Rectangle.BOTTOM) &&
                    (bWidth > 0)) {
                os.write(RtfWriter.escape);
                os.write(cellBorderBottom);
                os.write(RtfWriter.escape);
                os.write(bStyle);
                os.write(RtfWriter.escape);
                os.write(RtfRow.tableBorderWidth);
                writeInt(os, (int) (bWidth * RtfWriter.TWIPSFACTOR));
                os.write(RtfWriter.escape);
                os.write(RtfRow.tableBorderColor);
                if (store.getBorderColor() == null)
                    writeInt(os, writer.addColor(new
                            Color(0, 0, 0)));
                else
                    writeInt(os, writer.addColor(store.getBorderColor()));
                os.write((byte) '\n');
            }
            if (((store.getBorder() & Rectangle.RIGHT) == Rectangle.RIGHT) &&
                    (rWidth > 0)) {
                os.write(RtfWriter.escape);
                os.write(cellBorderRight);
                os.write(RtfWriter.escape);
                os.write(rStyle);
                os.write(RtfWriter.escape);
                os.write(RtfRow.tableBorderWidth);
                writeInt(os, (int) (rWidth * RtfWriter.TWIPSFACTOR));
                os.write(RtfWriter.escape);
                os.write(RtfRow.tableBorderColor);
                if (store.getBorderColor() == null)
                    writeInt(os, writer.addColor(new
                            Color(0, 0, 0)));
                else
                    writeInt(os, writer.addColor(store.getBorderColor()));
                os.write((byte) '\n');
            }
            os.write(RtfWriter.escape);
            os.write(cellBackgroundColor);
            if (store.getBackgroundColor() == null) {
                writeInt(os, writer.addColor(new Color(255, 255, 255)));
            } else {
                writeInt(os, writer.addColor(store.getBackgroundColor()));
            }
            os.write((byte) '\n');
            os.write(RtfWriter.escape);
            os.write(cellWidthStyle);
            os.write((byte) '\n');
            os.write(RtfWriter.escape);
            os.write(cellWidthTag);
            writeInt(os, cellWidth);
            os.write((byte) '\n');
            if (cellpadding > 0) {
                
                os.write(RtfWriter.escape);
                os.write(cellPaddingLeft);
                writeInt(os, cellpadding / 2);
                os.write(RtfWriter.escape);
                os.write(cellPaddingTop);
                writeInt(os, cellpadding / 2);
                os.write(RtfWriter.escape);
                os.write(cellPaddingRight);
                writeInt(os, cellpadding / 2);
                os.write(RtfWriter.escape);
                os.write(cellPaddingBottom);
                writeInt(os, cellpadding / 2);
                
                os.write(RtfWriter.escape);
                os.write(cellPaddingLeftUnit);
                os.write(RtfWriter.escape);
                os.write(cellPaddingTopUnit);
                os.write(RtfWriter.escape);
                os.write(cellPaddingRightUnit);
                os.write(RtfWriter.escape);
                os.write(cellPaddingBottomUnit);
            }
            os.write(RtfWriter.escape);
            os.write(cellRightBorder);
            writeInt(os, cellRight);
        } catch (IOException e) {
            return false;
        }
        return true;
    }

    
    public boolean writeCellContent(ByteArrayOutputStream os) throws DocumentException {
        try {
            if (mergeType == MERGE_HORIZ_PREV || mergeType == MERGE_BOTH_PREV) {
                return true;
            }
            
            if (!emptyCell) {
                Iterator<Element> cellIterator = store.getElements();
                Paragraph container = null;
                while (cellIterator.hasNext()) {
                    Element element = cellIterator.next();
                    
                    if(!(element instanceof Paragraph)) {
                        if(container != null) {
                            container.add(element);
                        } else {
                            container = new Paragraph();
                            container.setAlignment(store.getHorizontalAlignment());
                            container.add(element);
                        }
                    } else {
                        if(container != null) {
                            writer.addElement(container, os);
                            container = null;
                        }
                        
                        
                        
                        
                        if (element instanceof Paragraph && ((Paragraph) element).getAlignment() == Element.ALIGN_UNDEFINED) {
                            ((Paragraph) element).setAlignment(store.getHorizontalAlignment());
                        }
                        writer.addElement(element, os);
                        if (element.type() == Element.PARAGRAPH && cellIterator.hasNext()) {
                            os.write(RtfWriter.escape);
                            os.write(RtfWriter.paragraph);
                        }
                    }
                }
                if(container != null) {
                    writer.addElement(container, os);
                    container =null;
                }
            } else {
                os.write(RtfWriter.escape);
                os.write(RtfWriter.paragraphDefaults);
                os.write(RtfWriter.escape);
                os.write(cellInTable);
            }
            os.write(RtfWriter.escape);
            os.write(cellEnd);
        } catch (IOException e) {
            return false;
        }
        return true;
    }

    
    public void setMerge(int mergeType, RtfCell mergeCell) {
        this.mergeType = mergeType;
        store = mergeCell.getStore();
    }

    
    public Cell getStore() {
        return store;
    }

    
    public int getCellWidth() {
        return cellWidth;
    }

    
    public void setCellWidth(int value) {
        cellWidth = value;
    }

    
    public int getCellRight() {
        return cellRight;
    }


    
    public void setCellRight(int value) {
        cellRight = value;
    }

    
    private void writeInt(ByteArrayOutputStream out, int i) throws IOException {
        out.write(Integer.toString(i).getBytes());
    }
}
