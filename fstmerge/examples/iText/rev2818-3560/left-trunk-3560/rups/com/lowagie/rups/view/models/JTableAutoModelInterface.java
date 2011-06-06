

package com.lowagie.rups.view.models;


public interface JTableAutoModelInterface {
    
    public int getColumnCount();
     
    public int getRowCount();
    
    public String getColumnName(int columnIndex);
    
    public Object getValueAt(int rowIndex, int columnIndex);
}
