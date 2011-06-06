

package com.lowagie.text.rtf;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.DocumentException;
import com.lowagie.text.Element;
import com.lowagie.text.Row;
import com.lowagie.text.Table;


public class RtfTable {
    
    private ArrayList<RtfRow> rowsList = new ArrayList<RtfRow>();
    
    private RtfWriter writer = null;
    
    private Table origTable = null;



    
    public RtfTable(RtfWriter writer) {
        super();
        this.writer = writer;
    }

    
    public boolean importTable(Table table, int pageWidth) {
        origTable = table;
        
        Iterator<Row> rows = table.iterator();
        Row row = null;

        int tableWidth = (int) table.getWidth();
        int cellpadding = (int) (table.getPadding() * RtfWriter.TWIPSFACTOR);
        int cellspacing = (int) (table.getSpacing() * RtfWriter.TWIPSFACTOR);
        float[] propWidths = table.getProportionalWidths();

        int borders = table.getBorder();
        java.awt.Color borderColor = table.getBorderColor();
        float borderWidth = table.getBorderWidth();

        for (int i = 0; i < table.size(); i++) {
            RtfRow rtfRow = new RtfRow(writer, this);
            rtfRow.pregenerateRows(table.getColumns());
            rowsList.add(rtfRow);
        }
        int i = 0;
        while (rows.hasNext()) {
            row = rows.next();
            row.setHorizontalAlignment(table.getAlignment());
            RtfRow rtfRow = rowsList.get(i);
            rtfRow.importRow(row, propWidths, tableWidth, pageWidth, cellpadding, cellspacing, borders, borderColor, borderWidth, i);
            i++;
        }
        return true;
    }

    
    public boolean writeTable(ByteArrayOutputStream os) throws DocumentException, IOException {
        
        if(!this.writer.writingHeaderFooter()) {
            
            
            os.write(RtfWriter.escape);
            os.write(RtfWriter.paragraph);
        }
            
        int size = rowsList.size();
        for (int i = 0; i < size; i++) {
            RtfRow row = rowsList.get(i);
            row.writeRow(os, i, origTable);
            os.write((byte) '\n');
        }
        if (!writer.writingHeaderFooter()) {
            os.write(RtfWriter.escape);
            os.write(RtfWriter.paragraphDefaults);
            os.write(RtfWriter.escape);
            os.write(RtfWriter.paragraph);
            switch (origTable.getAlignment()) {
                case Element.ALIGN_LEFT:
                    os.write(RtfWriter.escape);
                    os.write(RtfWriter.alignLeft);
                    break;
                case Element.ALIGN_RIGHT:
                    os.write(RtfWriter.escape);
                    os.write(RtfWriter.alignRight);
                    break;
                case Element.ALIGN_CENTER:
                    os.write(RtfWriter.escape);
                    os.write(RtfWriter.alignCenter);
                    break;
                case Element.ALIGN_JUSTIFIED:
                case Element.ALIGN_JUSTIFIED_ALL:
                    os.write(RtfWriter.escape);
                    os.write(RtfWriter.alignJustify);
                    break;
            }
        }
        return true;
    }

    
    public void setMerge(int x, int y, int mergeType, RtfCell mergeCell) {
        RtfRow row = rowsList.get(y);
        row.setMerge(x, mergeType, mergeCell);
    }

    
    protected Table getOriginalTable() {
        return origTable;
    }
}
