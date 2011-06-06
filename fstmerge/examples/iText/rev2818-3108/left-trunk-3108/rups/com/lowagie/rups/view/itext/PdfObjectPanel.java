

package com.lowagie.rups.view.itext;
import java.awt.CardLayout;
import java.util.Observable;
import java.util.Observer;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;

import com.lowagie.rups.view.models.DictionaryTableModel;
import com.lowagie.rups.view.models.PdfArrayTableModel;
import com.lowagie.text.pdf.PdfArray;
import com.lowagie.text.pdf.PdfDictionary;
import com.lowagie.text.pdf.PdfObject;

public class PdfObjectPanel extends JPanel implements Observer {

    
    private static final String TEXT = "text";
    
    private static final String TABLE = "table";
    
    
    protected CardLayout layout = new CardLayout();

    
    JTable table = new JTable();
    
    JTextArea text = new JTextArea();
    
    
    public PdfObjectPanel() {
        
        setLayout(layout);

        
        JScrollPane dict_scrollpane = new JScrollPane();
        dict_scrollpane.setViewportView(table);
        add(dict_scrollpane, TABLE);
        
        
        JScrollPane text_scrollpane = new JScrollPane();
        text_scrollpane.setViewportView(text);
        add(text_scrollpane, TEXT);
    }
    
    
    public void clear() {
        text.setText(null);
        layout.show(this, TEXT);
    }

    
    public void update(Observable observable, Object obj) {
        clear();
    }
    
    
    public void render(PdfObject object) {
        if (object == null) {
            text.setText(null);
            layout.show(this, TEXT);
            this.repaint();
            text.repaint();
            return;
        }
        switch(object.type()) {
        case PdfObject.DICTIONARY:
        case PdfObject.STREAM:
            table.setModel(new DictionaryTableModel((PdfDictionary)object));
            layout.show(this, TABLE);
            this.repaint();
            break;
        case PdfObject.ARRAY:
            table.setModel(new PdfArrayTableModel((PdfArray)object));
            layout.show(this, TABLE);
            this.repaint();
            break;
        default:
            text.setText(object.toString());
            layout.show(this, TEXT);
            break;
        }
    }
    
    
    private static final long serialVersionUID = 1302283071087762494L;

}
