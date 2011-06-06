

package com.lowagie.text.html.simpleparser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;

import com.lowagie.text.Chunk;
import com.lowagie.text.Element;
import com.lowagie.text.ElementListener;
import com.lowagie.text.pdf.PdfPCell;
import com.lowagie.text.pdf.PdfPTable;


public class IncTable implements Element {
    private ArrayList<Chunk> chunks = new ArrayList<Chunk>();

    private HashMap<String, String> props = new HashMap<String, String>();
    private ArrayList<ArrayList<PdfPCell>> rows = new ArrayList<ArrayList<PdfPCell>>();
    private ArrayList<PdfPCell> cols;
    
    public IncTable(HashMap<String, String> props) {
        this.props.putAll(props);
    }
    
    public void addCol(PdfPCell cell) {
        if (cols == null)
            cols = new ArrayList<PdfPCell>();
        cols.add(cell);
    }
    
    public void addCols(ArrayList<PdfPCell> ncols) {
        if (cols == null)
            cols = new ArrayList<PdfPCell>(ncols);
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
    
    public ArrayList<ArrayList<PdfPCell>> getRows() {
        return rows;
    }
    
    public PdfPTable buildTable() {
        if (rows.isEmpty())
            return new PdfPTable(1);
        int ncol = 0;
        ArrayList<PdfPCell> c0 = rows.get(0);
        for (int k = 0; k < c0.size(); ++k) {
            ncol += c0.get(k).getColspan();
        }
        PdfPTable table = new PdfPTable(ncol);
        String width = props.get("width");
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
            ArrayList<PdfPCell> col = rows.get(row);
            for (int k = 0; k < col.size(); ++k) {
                table.addCell(col.get(k));
            }
        }
        return table;
    }

    
    public ArrayList<Chunk> getChunks() {
        return chunks;
    }
    
    public boolean process(ElementListener listener) {
        return true;
    }
    
    public int type() {
        return 0;
    }
    
    public boolean isNestable() { return true; }

    public boolean isContent() { return true; }

}
