package net.sf.jabref.gui;

import javax.swing.JTable;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import net.sf.jabref.Globals;
import net.sf.jabref.Util;


public class PreventDraggingJTableHeader extends JTableHeader {

    public PreventDraggingJTableHeader(TableColumnModel cm) {
        super(cm);
    }

    
    @Override
    public void setDraggedColumn(TableColumn column) {

        if (column != null) {

            
            if (column.getModelIndex() == 0) {
                return;
            }

            
            if (isUnnamed(column)) {
                return;
            }
        }

        super.setDraggedColumn(column);
    }

    
    @Override
    public TableColumn getDraggedColumn() {
        TableColumn column = super.getDraggedColumn();
        if (column != null) {
            preventDragBeforeIndex(this.getTable(), column.getModelIndex(),
                    getSpecialColumnsCount());
        }

        return column;
    }

    
    private int getSpecialColumnsCount() {
        int count = 0;
        if (Globals.prefs.getBoolean("fileColumn")) {
            count++;
        }
        if (Globals.prefs.getBoolean("pdfColumn")) {
            count++;
        }
        if (Globals.prefs.getBoolean("urlColumn")) {
            ;
            count++;
        }
        if (Globals.prefs.getBoolean("citeseerColumn")) {
            count++;
        }
        return count;
    }

    private static boolean isUnnamed(TableColumn column) {
        return column.getHeaderValue() == null
                || "".equals(column.getHeaderValue().toString());
    }

    
    private static void preventDragBeforeIndex(JTable table, int mColIndex,
            int toIndex) {

        for (int c = 0; c < table.getColumnCount(); c++) {

            TableColumn col = table.getColumnModel().getColumn(c);

            
            
            if (col.getModelIndex() == mColIndex && c <= toIndex) {
                
                

                
                table.getColumnModel().moveColumn(toIndex, toIndex + 1);
                return; 
            }

        }
    }
}
