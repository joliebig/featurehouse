

package com.lowagie.rups.view.models;

import javax.swing.table.AbstractTableModel;

import com.lowagie.text.pdf.PdfArray;


public class PdfArrayTableModel extends AbstractTableModel {
    
    
    private static final long serialVersionUID = 4665485782853993708L;
    
    protected PdfArray array;

    
    public PdfArrayTableModel(PdfArray array) {
        this.array = array;
    }
    
    
    public int getColumnCount() {
        return 1;
    }

    
    public int getRowCount() {
        return array.size();
    }

    
    public Object getValueAt(int rowIndex, int columnIndex) {
        switch (columnIndex) {
        case 0:
            return array.getPdfObject(rowIndex);
        default:
            return null;
        }
    }

    
    public String getColumnName(int columnIndex) {
        switch (columnIndex) {
        case 0:
            return "Array";
        default:
            return null;
        }
    }

}

