

package com.lowagie.rups.view.itext;

import java.util.Observable;
import java.util.Observer;

import javax.swing.JTree;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import com.lowagie.rups.view.icons.IconTreeCellRenderer;
import com.lowagie.rups.view.itext.treenodes.PdfObjectTreeNode;
import com.lowagie.rups.view.itext.treenodes.PdfTrailerTreeNode;


public class PdfTree extends JTree implements Observer {

    
    protected PdfTrailerTreeNode root;
    
    
    public PdfTree() {
        super();
        root = new PdfTrailerTreeNode();
        setCellRenderer(new IconTreeCellRenderer());
        update(null, null);
    }
    
    
    public PdfTrailerTreeNode getRoot() {
        return root;
    }

    
    public void update(Observable observable, Object obj) {
        if (obj == null) {
            root = new PdfTrailerTreeNode();
        }
        setModel(new DefaultTreeModel(root));
        repaint();
        return;
    }

    
    public void selectNode(PdfObjectTreeNode node) {
        TreePath path = new TreePath(node.getPath());
        setSelectionPath(path);
        scrollPathToVisible(path);
    }

    
    private static final long serialVersionUID = 7545804447512085734L;
    
}