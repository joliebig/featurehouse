

package com.lowagie.rups.view.itext;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Observable;
import java.util.Observer;

import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;

import com.lowagie.rups.controller.PdfReaderController;
import com.lowagie.rups.model.ObjectLoader;
import com.lowagie.rups.model.TreeNodeFactory;
import com.lowagie.rups.view.PageNavigationListener;
import com.lowagie.rups.view.itext.treenodes.PdfObjectTreeNode;
import com.lowagie.rups.view.itext.treenodes.PdfPageTreeNode;
import com.lowagie.rups.view.itext.treenodes.PdfPagesTreeNode;
import com.lowagie.rups.view.itext.treenodes.PdfTrailerTreeNode;
import com.lowagie.rups.view.models.JTableAutoModel;
import com.lowagie.rups.view.models.JTableAutoModelInterface;
import com.lowagie.text.pdf.PdfName;
import com.lowagie.text.pdf.PdfPageLabels;


public class PagesTable extends JTable implements JTableAutoModelInterface, Observer {

    
    protected ArrayList<PdfPageTreeNode> list = new ArrayList<PdfPageTreeNode>();
    
    protected PdfReaderController controller;
    
    protected PageNavigationListener listener;

    
    public PagesTable(PdfReaderController controller, PageNavigationListener listener) {
        this.controller = controller;
        this.listener = listener;
    }
    
    
    public void update(Observable observable, Object obj) {
        if (obj == null) {
            list = new ArrayList<PdfPageTreeNode>();
            repaint();
            return;
        }
        if (obj instanceof ObjectLoader) {
            ObjectLoader loader = (ObjectLoader)obj;
            String[] pagelabels = PdfPageLabels.getPageLabels(loader.getReader());
            int i = 0;
            TreeNodeFactory factory = loader.getNodes();
            PdfTrailerTreeNode trailer = controller.getPdfTree().getRoot();
            PdfObjectTreeNode catalog = factory.getChildNode(trailer, PdfName.ROOT);
            PdfPagesTreeNode pages = (PdfPagesTreeNode)factory.getChildNode(catalog, PdfName.PAGES);
            if (pages == null) {
                return;
            }
            Enumeration p = pages.depthFirstEnumeration();
            PdfObjectTreeNode  child;
            StringBuffer buf;
            while (p.hasMoreElements()) {
                child = (PdfObjectTreeNode)p.nextElement();
                if (child instanceof PdfPageTreeNode) {
                    buf = new StringBuffer("Page ");
                    buf.append(++i);
                    if (pagelabels != null) {
                        buf.append(" ( ");
                        buf.append(pagelabels[i - 1]);
                        buf.append(" )");
                    }
                    child.setUserObject(buf.toString());
                    list.add((PdfPageTreeNode)child);
                }
            }
        }
        setModel(new JTableAutoModel(this));
    }
    
    
    public int getColumnCount() {
        return 2;
    }
    
    
    public int getRowCount() {
        return list.size();
    }

    
    public Object getValueAt(int rowIndex, int columnIndex) {
        if (getRowCount() == 0) return null;
        switch (columnIndex) {
        case 0:
            return "Object " + list.get(rowIndex).getNumber();
        case 1:
            return list.get(rowIndex);
        }
        return null;
    }
    
    public String getColumnName(int columnIndex) {
        switch (columnIndex) {
        case 0:
            return "Object";
        case 1:
            return "Page";
        default:
            return null;
        }
    }

    
    @Override
    public void valueChanged(ListSelectionEvent evt) {
        if (evt != null)
            super.valueChanged(evt);
        if (controller == null)
            return;
        if (getRowCount() > 0) {
            controller.selectNode(list.get(getSelectedRow()));
            if (listener != null)
                listener.gotoPage(getSelectedRow() + 1);
        }
    }

    
    private static final long serialVersionUID = -6523261089453886508L;

}