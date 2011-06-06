

package com.lowagie.text.html.simpleparser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;

import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;


public class IncTable {
    private HashMap props = new HashMap();
    private ArrayList rows = new ArrayList();
    private ArrayList cols;
    
    public IncTable(HashMap props) {
        this.props.putAll(props);
    }
    
    public void addCol(PdfPCell cell) {
        if (cols == null)
            cols = new ArrayList();
        cols.add(cell);
    }
    
    public void addCols(ArrayList ncols) {
        if (cols == null)
            cols = new ArrayList(ncols);
        else
            cols.addAll(ncols);
    }
    
    public void endRow() {
        if (cols != null) {
            Collections.reverse(cols);
            rows.add(cols);
            cols = null;
        }
    }
    
    public ArrayList getRows() {
        return rows;
    }
    
    public PdfPTable buildTable() {
        if (rows.isEmpty())
            return new PdfPTable(1);
        int ncol = 0;
        ArrayList c0 = (ArrayList)rows.get(0);
        for (int k = 0; k < c0.size(); ++k) {
            ncol += ((PdfPCell)c0.get(k)).getColspan();
        }
        PdfPTable table = new PdfPTable(ncol);
        String width = (String)props.get("width");
        if (width == null)
            table.setWidthPercentage(100);
        else {
            if (width.endsWith("%"))
                table.setWidthPercentage(Float.parseFloat(width.substring(0, width.length() - 1)));
            else {
                table.setTotalWidth(Float.parseFloat(width));
                table.setLockedWidth(true);
            }
        }
        for (int row = 0; row < rows.size(); ++row) {
            ArrayList col = (ArrayList)rows.get(row);
            for (int k = 0; k < col.size(); ++k) {
                table.addCell((PdfPCell)col.get(k));
            }
        }
        return table;
    }
}
