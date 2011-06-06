

package com.lowagie.rups.view.models;

import javax.swing.table.AbstractTableModel;


public class JTableAutoModel extends AbstractTableModel {
    
    
    private static final long serialVersionUID = -2229431581745521537L;
    
    protected JTableAutoModelInterface table;

    
    public JTableAutoModel(JTableAutoModelInterface table) {
        this.table = table;
    }
    
    
    public int getColumnCount() {
        return table.getColumnCount();
    }

    
    public int getRowCount() {
        return table.getRowCount();
    }

    
    public String getColumnName(int columnIndex) {
        return table.getColumnName(columnIndex);
    }

    
    public Object getValueAt(int rowIndex, int columnIndex) {
        return table.getValueAt(rowIndex, columnIndex);
    }
}
