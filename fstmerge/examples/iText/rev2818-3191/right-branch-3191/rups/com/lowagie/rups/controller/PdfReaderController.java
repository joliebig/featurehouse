

package com.lowagie.rups.controller;

import java.util.Observable;
import java.util.Observer;

import javax.swing.JTabbedPane;
import javax.swing.event.TreeSelectionListener;

import com.lowagie.rups.model.ObjectLoader;
import com.lowagie.rups.model.PdfFile;
import com.lowagie.rups.model.TreeNodeFactory;
import com.lowagie.rups.view.PageNavigationListener;
import com.lowagie.rups.view.RupsMenuBar;
import com.lowagie.rups.view.Utilities;
import com.lowagie.rups.view.itext.FormTree;
import com.lowagie.rups.view.itext.OutlineTree;
import com.lowagie.rups.view.itext.PagesTable;
import com.lowagie.rups.view.itext.PdfObjectPanel;
import com.lowagie.rups.view.itext.PdfTree;
import com.lowagie.rups.view.itext.StreamTextArea;
import com.lowagie.rups.view.itext.XRefTable;
import com.lowagie.rups.view.itext.treenodes.PdfObjectTreeNode;
import com.lowagie.rups.view.itext.treenodes.PdfTrailerTreeNode;
import com.lowagie.text.pdf.PRStream;
import com.lowagie.text.pdf.PdfObject;


public class PdfReaderController extends Observable implements Observer {

    
    protected PdfTree pdfTree;
    
    protected JTabbedPane navigationTabs;
    
    protected PagesTable pages;
    
    protected OutlineTree outlines;
    
    protected FormTree form;
    
    protected XRefTable xref;
    
    protected PdfObjectPanel objectPanel;
    
    protected JTabbedPane editorTabs;
    
    protected StreamTextArea streamArea;
    
    
    protected TreeNodeFactory nodes;
    
    
    public PdfReaderController(TreeSelectionListener treeSelectionListener,
            PageNavigationListener pageNavigationListener) {
        pdfTree = new PdfTree();
        pdfTree.addTreeSelectionListener(treeSelectionListener);
        addObserver(pdfTree);
        pages = new PagesTable(this, pageNavigationListener);
        addObserver(pages);
        outlines = new OutlineTree(this);
        addObserver(outlines);
        form = new FormTree(this);
        addObserver(form);
        xref = new XRefTable(this);
        addObserver(xref);
        navigationTabs = new JTabbedPane();
        navigationTabs.addTab("Pages", null, Utilities.getScrollPane(pages), "Pages");
        navigationTabs.addTab("Outlines", null, Utilities.getScrollPane(outlines), "Outlines (Bookmarks)");
        navigationTabs.addTab("Form", null, Utilities.getScrollPane(form), "Interactive Form");
        navigationTabs.addTab("XFA", null, form.getXfaTree(), "Tree view of the XFA form");
        navigationTabs.addTab("XRef", null, Utilities.getScrollPane(xref), "Cross-reference table");
        objectPanel = new PdfObjectPanel();
        addObserver(objectPanel);
        streamArea = new StreamTextArea();
        addObserver(streamArea);
        editorTabs = new JTabbedPane();
        editorTabs.addTab("Stream", null, streamArea, "Stream");
        editorTabs.addTab("XFA", null, form.getXfaTextArea(), "XFA Form XML file");
    }

    
    public PdfTree getPdfTree() {
        return pdfTree;
    }

    
    public JTabbedPane getNavigationTabs() {
        return navigationTabs;
    }

    
    public PdfObjectPanel getObjectPanel() {
        return objectPanel;
    }

    
    public JTabbedPane getEditorTabs() {
        return editorTabs;
    }

    
    public StreamTextArea getStreamArea() {
        return streamArea;
    }
    
    
    public void startObjectLoader(PdfFile file) {
        setChanged();
        notifyObservers();
        setChanged();
        new ObjectLoader(this, file.getPdfReader());
    }

    
    @Override
    public void notifyObservers(Object obj) {
        if (obj instanceof ObjectLoader) {
            ObjectLoader loader = (ObjectLoader)obj;
            nodes = loader.getNodes();
            PdfTrailerTreeNode root = pdfTree.getRoot();
            root.setTrailer(loader.getReader().getTrailer());
            root.setUserObject("PDF Object Tree");
            nodes.expandNode(root);
        }
        super.notifyObservers(obj);
    }
    
    
    public void selectNode(PdfObjectTreeNode node) {
        pdfTree.selectNode(node);
    }

    
    public void selectNode(int objectNumber) {
        selectNode(nodes.getNode(objectNumber));
    }

    
    public void render(PdfObject object) {
        objectPanel.render(object);
        streamArea.render(object);
        if (object instanceof PRStream) {
            editorTabs.setSelectedComponent(streamArea);
        }
        else {
            editorTabs.setSelectedIndex(editorTabs.getComponentCount() - 1);
        }
    }

    
    public void gotoPage(int pageNumber) {
        pageNumber--;
        if (pages == null || pages.getSelectedRow() == pageNumber)
            return;
        if (pageNumber < pages.getRowCount())
            pages.setRowSelectionInterval(pageNumber, pageNumber);
    }

    
    public void update(Observable observable, Object obj) {
        if (RupsMenuBar.CLOSE.equals(obj)) {
            setChanged();
            notifyObservers(null);
            nodes = null;
        }
        if (obj instanceof PdfObjectTreeNode) {
            PdfObjectTreeNode node = (PdfObjectTreeNode)obj;
            nodes.expandNode(node);
            if (node.isRecursive()) {
                pdfTree.selectNode(node.getAncestor());
                return;
            }
            if (node.isIndirect()) {
                xref.selectRowByReference(node.getNumber());
                return;
            }
            render(node.getPdfObject());
        }
    }
}
