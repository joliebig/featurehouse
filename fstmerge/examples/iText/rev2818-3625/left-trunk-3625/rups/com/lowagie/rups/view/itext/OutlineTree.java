

package com.lowagie.rups.view.itext;

import java.util.Observable;
import java.util.Observer;

import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultTreeModel;

import com.lowagie.rups.controller.PdfReaderController;
import com.lowagie.rups.model.ObjectLoader;
import com.lowagie.rups.model.TreeNodeFactory;
import com.lowagie.rups.view.icons.IconTreeCellRenderer;
import com.lowagie.rups.view.itext.treenodes.OutlineTreeNode;
import com.lowagie.rups.view.itext.treenodes.PdfObjectTreeNode;
import com.lowagie.rups.view.itext.treenodes.PdfTrailerTreeNode;
import com.lowagie.text.pdf.PdfName;


public class OutlineTree extends JTree implements TreeSelectionListener, Observer {

    
    protected PdfReaderController controller;
    
    
    public OutlineTree(PdfReaderController controller) {
        super();
        this.controller = controller;
        setCellRenderer(new IconTreeCellRenderer());
        setModel(new DefaultTreeModel(new OutlineTreeNode()));
        addTreeSelectionListener(this);
    }

    
    public void update(Observable observable, Object obj) {
        if (obj == null) {
            setModel(new DefaultTreeModel(new OutlineTreeNode()));
            repaint();
            return;
        }
        if (obj instanceof ObjectLoader) {
            ObjectLoader loader = (ObjectLoader)obj;
            TreeNodeFactory factory = loader.getNodes();
            PdfTrailerTreeNode trailer = controller.getPdfTree().getRoot();
            PdfObjectTreeNode catalog = factory.getChildNode(trailer, PdfName.ROOT);
            PdfObjectTreeNode outline = factory.getChildNode(catalog, PdfName.OUTLINES);
            if (outline == null) {
                return;
            }
            OutlineTreeNode root = new OutlineTreeNode();
            loadOutline(factory, root, factory.getChildNode(outline, PdfName.FIRST));
            setModel(new DefaultTreeModel(root));
        }
    }
    
    
    private void loadOutline(TreeNodeFactory factory, OutlineTreeNode parent, PdfObjectTreeNode child) {
        OutlineTreeNode childnode = new OutlineTreeNode(child);
        parent.add(childnode);
        PdfObjectTreeNode first = factory.getChildNode(child, PdfName.FIRST);
        if (first != null) {
            loadOutline(factory, childnode, first);
        }
        PdfObjectTreeNode next = factory.getChildNode(child, PdfName.NEXT);
        if (next != null) {
            loadOutline(factory, parent, next);
        }
    }

    
    public void valueChanged(TreeSelectionEvent evt) {
        if (controller == null)
            return;
        OutlineTreeNode selectednode = (OutlineTreeNode)this.getLastSelectedPathComponent();
        PdfObjectTreeNode node = selectednode.getCorrespondingPdfObjectNode();
        if (node != null)
            controller.selectNode(node);
    }

    
    private static final long serialVersionUID = 5646572654823301007L;

}
