

package com.lowagie.text.rtf;

import java.awt.Color;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.Cell;
import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Rectangle;
import com.lowagie.text.Row;
import com.lowagie.text.Table;


public class RtfRow {
    
    public static final byte[] tableBorder = "brdrs".getBytes();
    
    public static final byte[] tableBorderWidth = "brdrw".getBytes();
    
    public static final byte[] tableBorderColor = "brdrcf".getBytes();

    
    private static final byte[] rowBegin = "trowd".getBytes();
    
    private static final byte[] rowEnd = "row".getBytes();
    
    private static final byte[] rowAutofit = "trautofit1".getBytes();
    private static final byte[] graphLeft = "trgaph".getBytes();
    
    private static final byte[] rowBorderLeft = "trbrdrl".getBytes();
    
    private static final byte[] rowBorderRight = "trbrdrr".getBytes();
    
    private static final byte[] rowBorderTop = "trbrdrt".getBytes();
    
    private static final byte[] rowBorderBottom = "trbrdrb".getBytes();
    
    private static final byte[] rowBorderInlineHorizontal = "trbrdrh".getBytes();
    
    private static final byte[] rowBorderInlineVertical = "trbrdrv".getBytes();
    
    private static final byte[] rowSpacingLeft = "trspdl".getBytes();
    
    private static final byte[] rowSpacingRight = "trspdr".getBytes();
    
    private static final byte[] rowSpacingTop = "trspdt".getBytes();
    
    private static final byte[] rowSpacingBottom = "trspdb".getBytes();
    
    private static final byte[] rowSpacingLeftStyle = "trspdfl3".getBytes();
    
    private static final byte[] rowSpacingRightStyle = "trspdfr3".getBytes();
    
    private static final byte[] rowSpacingTopStyle = "trspdft3".getBytes();
    
    private static final byte[] rowSpacingBottomStyle = "trspdfb3".getBytes();
    
    private static final byte[] rowPaddingLeft = "trpaddl".getBytes();
    
    private static final byte[] rowPaddingRight = "trpaddr".getBytes();
    
    private static final byte[] rowPaddingLeftStyle = "trpaddfl3".getBytes();
    
    private static final byte[] rowPaddingRightStyle = "trpaddfr3".getBytes();
    
    private static final byte[] rowWidthStyle = "trftsWidth3".getBytes();
    
    private static final byte[] rowWidth = "trwWidth".getBytes();
    
    private static final byte[] rowHeader = "trhdr".getBytes();
    
    private static final byte[] rowKeep = "trkeep".getBytes();
    
    private static final byte[] rowAlignLeft = "trql".getBytes();
    
    private static final byte[] rowAlignCenter = "trqc".getBytes();
    
    private static final byte[] rowAlignRight = "trqr".getBytes();

    
    private ArrayList<RtfCell> cells = new ArrayList<RtfCell>();
    
    private RtfWriter writer = null;
    
    private RtfTable mainTable = null;

    
    private int width = 100;
    
    private int cellpadding = 115;
    
    private int cellspacing = 14;
    
    private int borders = 0;
    
    private java.awt.Color borderColor = null;
    
    private float borderWidth = 0;

    
    private Row origRow = null;

    
    public RtfRow(RtfWriter writer, RtfTable mainTable) {
        super();
        this.writer = writer;
        this.mainTable = mainTable;
    }

    
    public void pregenerateRows(int columns) {
        for (int i = 0; i < columns; i++) {
            RtfCell rtfCell = new RtfCell(writer, mainTable);
            cells.add(rtfCell);
        }
    }

    
    public boolean importRow(Row row, float[] propWidths, int tableWidth, int pageWidth, int cellpadding,
                             int cellspacing, int borders, java.awt.Color borderColor, float borderWidth,
                             int y) {
        
        
        this.origRow = row;
        this.width = pageWidth * tableWidth / 100;
        this.cellpadding = cellpadding;
        this.cellspacing = cellspacing;
        this.borders = borders;
        this.borderColor = borderColor;
        this.borderWidth = borderWidth;

        if (this.borderWidth > 2) this.borderWidth = 2;

        int cellLeft = 0;
        for (int i = 0; i < row.getColumns(); i++) {
            Element cell = (Element) row.getCell(i);

            
            
            
            int cellWidth = (int) (width * propWidths[i] / 100);
            if (cell != null) {
                if (cell.type() == Element.CELL) {
                    RtfCell rtfCell = cells.get(i);
                    cellLeft = rtfCell.importCell((Cell) cell, cellLeft, cellWidth, i, y, cellpadding);
                }
            } else {
                RtfCell rtfCell = cells.get(i);
                cellLeft = rtfCell.importCell(null, cellLeft, cellWidth, i, y, cellpadding);
            }
        }

        
        
        
        
        
        int columns = row.getColumns();
        for (int i = 0; i < columns; i++) {
            RtfCell firstCell = cells.get(i);
            Cell cell = firstCell.getStore();
            int cols = 0;
            if(cell != null) {
                cols = cell.getColspan();
            }
            if (cols > 1) {
                RtfCell lastCell = cells.get(i + cols - 1);
                firstCell.setCellRight(lastCell.getCellRight());
                int width = firstCell.getCellWidth();
                for (int j = i + 1; j < i + cols; j++) {
                    RtfCell cCell = cells.get(j);
                    width += cCell.getCellWidth();
                }
                firstCell.setCellWidth(width);
                i += cols - 1;
            }
        }
        return true;
    }

    
    public boolean writeRow(ByteArrayOutputStream os, int rowNum, Table table) throws DocumentException,
            IOException {
        os.write(RtfWriter.escape);
        os.write(rowBegin);
        os.write((byte) '\n');
        os.write(RtfWriter.escape);
        os.write(rowWidthStyle);
        os.write(RtfWriter.escape);
        os.write(rowWidth);
        writeInt(os, width);


        if (mainTable.getOriginalTable().isCellsFitPage()) {
            os.write(RtfWriter.escape);
            os.write(rowKeep);
        }
        
        if (rowNum < table.getLastHeaderRow() + 1) {
            os.write(RtfWriter.escape);
            os.write(rowHeader);
        }
        os.write(RtfWriter.escape);
        switch (this.origRow.getHorizontalAlignment()) {
            case Element.ALIGN_LEFT:
                os.write(rowAlignLeft);
                break;
            case Element.ALIGN_CENTER:
                os.write(rowAlignCenter);
                break;
            case Element.ALIGN_RIGHT:
                os.write(rowAlignRight);
                break;
            default :
                os.write(rowAlignLeft);
                break;
        }
        os.write(RtfWriter.escape);
        os.write(graphLeft);
        writeInt(os, 10);
        if (((borders & Rectangle.LEFT) == Rectangle.LEFT) && (borderWidth > 0)) {
            writeBorder(os, rowBorderLeft);
        }
        if (((borders & Rectangle.TOP) == Rectangle.TOP) && (borderWidth > 0)) {
            writeBorder(os, rowBorderTop);
        }
        if (((borders & Rectangle.BOTTOM) == Rectangle.BOTTOM) && (borderWidth > 0)) {
            writeBorder(os, rowBorderBottom);
        }
        if (((borders & Rectangle.RIGHT) == Rectangle.RIGHT) && (borderWidth > 0)) {
            writeBorder(os, rowBorderRight);
        }
        if (((borders & Rectangle.BOX) == Rectangle.BOX) && (borderWidth > 0)) {
            writeBorder(os, rowBorderInlineHorizontal);
            writeBorder(os, rowBorderInlineVertical);
        }

        if (cellspacing > 0) {
            os.write(RtfWriter.escape);
            os.write(rowSpacingLeft);
            writeInt(os, cellspacing / 2);
            os.write(RtfWriter.escape);
            os.write(rowSpacingLeftStyle);
            os.write(RtfWriter.escape);
            os.write(rowSpacingTop);
            writeInt(os, cellspacing / 2);
            os.write(RtfWriter.escape);
            os.write(rowSpacingTopStyle);
            os.write(RtfWriter.escape);
            os.write(rowSpacingBottom);
            writeInt(os, cellspacing / 2);
            os.write(RtfWriter.escape);
            os.write(rowSpacingBottomStyle);
            os.write(RtfWriter.escape);
            os.write(rowSpacingRight);
            writeInt(os, cellspacing / 2);
            os.write(RtfWriter.escape);
            os.write(rowSpacingRightStyle);
        }
        os.write(RtfWriter.escape);
        os.write(rowPaddingLeft);
        writeInt(os, cellpadding / 2);
        os.write(RtfWriter.escape);
        os.write(rowPaddingRight);
        writeInt(os, cellpadding / 2);
        os.write(RtfWriter.escape);
        os.write(rowPaddingLeftStyle);
        os.write(RtfWriter.escape);
        os.write(rowPaddingRightStyle);
        os.write((byte) '\n');

        Iterator<RtfCell> cellIterator = cells.iterator();
        while (cellIterator.hasNext()) {
            RtfCell cell = cellIterator.next();
            cell.writeCellSettings(os);
        }

        os.write(RtfWriter.escape);
        os.write("intbl".getBytes());

        cellIterator = cells.iterator();
        while (cellIterator.hasNext()) {
            RtfCell cell = cellIterator.next();
            cell.writeCellContent(os);
        }
        os.write(RtfWriter.delimiter);
        os.write(RtfWriter.escape);
        os.write(rowEnd);
        return true;
    }


    private void writeBorder(ByteArrayOutputStream os, byte[] borderType) throws IOException {
        
        os.write(RtfWriter.escape);
        os.write(borderType);
        
        os.write(RtfWriter.escape);
        os.write(RtfRow.tableBorder);
        
        os.write(RtfWriter.escape);
        os.write(RtfRow.tableBorderWidth);
        writeInt(os, (int) (borderWidth * RtfWriter.TWIPSFACTOR));
        
        os.write(RtfWriter.escape);
        os.write(RtfRow.tableBorderColor);
        if (borderColor == null) {
            writeInt(os, writer.addColor(new Color(0, 0, 0)));
        } else {
            writeInt(os, writer.addColor(borderColor));
        }
        os.write((byte) '\n');
    }


    
    public void setMerge(int x, int mergeType, RtfCell mergeCell) {
        RtfCell cell = cells.get(x);
        cell.setMerge(mergeType, mergeCell);
    }

    
    private void writeInt(ByteArrayOutputStream out, int i) throws IOException {
        out.write(Integer.toString(i).getBytes());
    }
}
