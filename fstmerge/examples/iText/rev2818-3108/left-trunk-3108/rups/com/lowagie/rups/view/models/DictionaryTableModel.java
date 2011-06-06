

package com.lowagie.rups.view.models;

import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.table.AbstractTableModel;

import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfName;


public class DictionaryTableModel extends AbstractTableModel {

    
    private static final long serialVersionUID = -8835275996639701776L;
    
    protected PdfDictionary dictionary;
    
    protected ArrayList<PdfName> keys = new ArrayList<PdfName>();

    
    public DictionaryTableModel(PdfDictionary dictionary) {
        this.dictionary = dictionary;
        for (Iterator i = dictionary.getKeys().iterator(); i.hasNext(); )
            this.keys.add((PdfName) i.next());
    }
    
    
    public int getColumnCount() {
        return 2;
    }

    
    public int getRowCount() {
        return dictionary.size();
    }

    
    public Object getValueAt(int rowIndex, int columnIndex) {
        switch (columnIndex) {
        case 0:
            return keys.get(rowIndex);
        case 1:
            return dictionary.get(keys.get(rowIndex));
        default:
            return null;
        }
    }

    
    public String getColumnName(int columnIndex) {
        switch (columnIndex) {
        case 0:
            return "Key";
        case 1:
            return "Value";
        default:
            return null;
        }
    }

}
