

package com.lowagie.text.rtf.table;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Iterator;

import com.lowagie.text.Element;
import com.lowagie.text.Row;
import com.lowagie.text.Table;
import com.lowagie.text.rtf.RtfElement;
import com.lowagie.text.rtf.document.RtfDocument;
import com.lowagie.text.rtf.text.RtfParagraph;



public class RtfTable extends RtfElement {

    
    private ArrayList<RtfRow> rows = null;
    
    private float tableWidthPercent = 80;
    
    private float[] proportionalWidths = null;
    
    private float cellPadding = 0;
    
    private float cellSpacing = 0;
    
    private RtfBorderGroup borders = null;
    
    private int alignment = Element.ALIGN_CENTER;
    
    private boolean cellsFitToPage = false;
    
    private boolean tableFitToPage = false;
    
    private int headerRows = 0;
    
    
    public RtfTable(RtfDocument doc, Table table) {
        super(doc);
        table.complete();
        importTable(table);
    }
    
    
    private void importTable(Table table) {
        this.rows = new ArrayList<RtfRow>();
        this.tableWidthPercent = table.getWidth();
        this.proportionalWidths = table.getProportionalWidths();
        this.cellPadding = (float) (table.getPadding() * TWIPS_FACTOR);
        this.cellSpacing = (float) (table.getSpacing() * TWIPS_FACTOR);
        this.borders = new RtfBorderGroup(this.document, RtfBorder.ROW_BORDER, table.getBorder(), table.getBorderWidth(), table.getBorderColor());
        this.alignment = table.getAlignment();
        
        int i = 0;
        Iterator<Row> rowIterator = table.iterator();
        while(rowIterator.hasNext()) {
            this.rows.add(new RtfRow(this.document, this, rowIterator.next(), i));
            i++;
        }
        for(RtfRow row: this.rows) {
            row.handleCellSpanning();
            row.cleanRow();
        }
        this.headerRows = table.getLastHeaderRow();
        this.cellsFitToPage = table.isCellsFitPage();
        this.tableFitToPage = table.isTableFitsPage();
    }
    
    
    public byte[] write() 
    {
        ByteArrayOutputStream result = new ByteArrayOutputStream();
        try {
            writeContent(result);
        } catch(IOException ioe) {
            ioe.printStackTrace();
        }
        return result.toByteArray();
    }
        
    public void writeContent(final OutputStream result) throws IOException
    {
        if(!inHeader) {
            result.write(RtfParagraph.PARAGRAPH);
        }
        
        for(int i = 0; i < this.rows.size(); i++) {
            RtfElement re = this.rows.get(i);
            
            re.writeContent(result);
        }
        
        result.write(RtfParagraph.PARAGRAPH_DEFAULTS);
    }        
    
    
    protected int getAlignment() {
        return alignment;
    }
    
    
    protected RtfBorderGroup getBorders() {
        return this.borders;
    }
    
    
    protected float getCellPadding() {
        return cellPadding;
    }
    
    
    protected float getCellSpacing() {
        return cellSpacing;
    }
    
    
    protected float[] getProportionalWidths() {
        return proportionalWidths.clone();
    }
    
    
    protected float getTableWidthPercent() {
        return tableWidthPercent;
    }
    
    
    protected ArrayList<RtfRow> getRows() {
        return this.rows;
    }
    
    
    protected boolean getCellsFitToPage() {
        return this.cellsFitToPage;
    }
    
    
    protected boolean getTableFitToPage() {
        return this.tableFitToPage;
    }
    
    
    protected int getHeaderRows() {
        return this.headerRows;
    }
}
