

package com.lowagie.rups.view.itext;

import javax.swing.JTree;
import javax.swing.tree.DefaultTreeModel;

import com.lowagie.rups.model.XfaFile;
import com.lowagie.rups.view.icons.IconTreeCellRenderer;
import com.lowagie.rups.view.icons.IconTreeNode;
import com.lowagie.rups.view.itext.treenodes.XdpTreeNode;


public class XfaTree extends JTree {

    
    public XfaTree() {
        super();
    }
    
    public void clear() {
        setCellRenderer(new IconTreeCellRenderer());
        setModel(new DefaultTreeModel(new IconTreeNode("xfa.png")));
    }
    public void load(XfaFile file) {
        setCellRenderer(new IconTreeCellRenderer());
        setModel(new DefaultTreeModel(new XdpTreeNode(file.getXfaDocument())));
    }

    
    private static final long serialVersionUID = -5072971223015095193L;

}