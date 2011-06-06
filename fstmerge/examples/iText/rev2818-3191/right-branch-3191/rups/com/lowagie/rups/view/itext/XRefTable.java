

package com.lowagie.rups.view.itext;

import java.util.Observable;
import java.util.Observer;

import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.table.TableColumn;

import com.lowagie.rups.controller.PdfReaderController;
import com.lowagie.rups.model.IndirectObjectFactory;
import com.lowagie.rups.model.ObjectLoader;
import com.lowagie.rups.view.models.JTableAutoModel;
import com.lowagie.rups.view.models.JTableAutoModelInterface;
import com.lowagie.text.pdf.PdfNull;
import com.lowagie.text.pdf.PdfObject;


public class XRefTable extends JTable implements JTableAutoModelInterface, Observer {

    
    protected IndirectObjectFactory objects;
    
    protected PdfReaderController controller;
    
    
    public XRefTable(PdfReaderController controller) {
        super();
        this.controller = controller;
    }
    
    
    public void update(Observable observable, Object obj) {
        if (obj == null) {
            objects = null;
            repaint();
            return;
        }
        if (observable instanceof PdfReaderController
                && obj instanceof ObjectLoader) {
            ObjectLoader loader = (ObjectLoader)obj;
            objects = loader.getObjects();
            setModel(new JTableAutoModel(this));
            TableColumn col= getColumnModel().getColumn(0);
            col.setPreferredWidth(5);
        }
    }
    
    
    public int getColumnCount() {
        return 2;
    }
    
    
    public int getRowCount() {
        if (objects == null) return 0;
        return objects.size();
    }

    
    public Object getValueAt(int rowIndex, int columnIndex) {
        switch (columnIndex) {
        case 0:
            return getObjectReferenceByRow(rowIndex);
        case 1:
            return getObjectDescriptionByRow(rowIndex);
        default:
            return null;
        }
    }
    
    
    protected int getObjectReferenceByRow(int rowIndex) {
        return objects.getRefByIndex(rowIndex);
    }
    
    
    protected String getObjectDescriptionByRow(int rowIndex) {
        PdfObject object = objects.getObjectByIndex(rowIndex);
        if (object instanceof PdfNull)
            return "Indirect object";
        return object.toString();
    }
    
    
    public String getColumnName(int columnIndex) {
        switch (columnIndex) {
        case 0:
            return "Number";
        case 1:
            return "Object";
        default:
            return null;
        }
    }
    
    
    protected PdfObject getObjectByRow(int rowIndex) {
        return objects.loadObjectByReference(getObjectReferenceByRow(rowIndex));
    }
    
    
    public void selectRowByReference(int ref) {
        int row = objects.getIndexByRef(ref);
        setRowSelectionInterval(row, row);
        scrollRectToVisible(getCellRect(row, 1, true));
        valueChanged(null);
    }

    
    @Override
    public void valueChanged(ListSelectionEvent evt) {
        if (evt != null)
            super.valueChanged(evt);
        if (controller != null && objects != null) {
            controller.render(getObjectByRow(this.getSelectedRow()));
            controller.selectNode(getObjectReferenceByRow(getSelectedRow()));
        }
    }
    
    
    private static final long serialVersionUID = -382184619041375537L;

}