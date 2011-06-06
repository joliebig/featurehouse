

package com.lowagie.rups.view.itext;

import java.io.IOException;
import java.util.Enumeration;
import java.util.Observable;
import java.util.Observer;

import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultTreeModel;

import org.dom4j.DocumentException;

import com.lowagie.rups.controller.PdfReaderController;
import com.lowagie.rups.model.ObjectLoader;
import com.lowagie.rups.model.TreeNodeFactory;
import com.lowagie.rups.model.XfaFile;
import com.lowagie.rups.view.icons.IconTreeCellRenderer;
import com.lowagie.rups.view.itext.treenodes.FormTreeNode;
import com.lowagie.rups.view.itext.treenodes.PdfObjectTreeNode;
import com.lowagie.rups.view.itext.treenodes.PdfTrailerTreeNode;
import com.lowagie.rups.view.itext.treenodes.XfaTreeNode;
import com.lowagie.text.pdf.PdfName;


public class FormTree extends JTree implements TreeSelectionListener, Observer {

    
    protected PdfReaderController controller;
    
    
    protected XfaFile xfaFile;
    
    protected XfaTree xfaTree;
    
    protected XfaTextArea xfaTextArea;
    
    
    public FormTree(PdfReaderController controller) {
        super();
        this.controller = controller;
        setCellRenderer(new IconTreeCellRenderer());
        setModel(new DefaultTreeModel(new FormTreeNode()));
        addTreeSelectionListener(this);
        xfaTree = new XfaTree();
        xfaTextArea = new XfaTextArea();
    }

    
    public void update(Observable observable, Object obj) {
        if (obj == null) {
            setModel(new DefaultTreeModel(new FormTreeNode()));
            xfaFile = null;
            xfaTree.clear();
            xfaTextArea.clear();
            repaint();
            return;
        }
        if (obj instanceof ObjectLoader) {
            ObjectLoader loader = (ObjectLoader)obj;
            TreeNodeFactory factory = loader.getNodes();
            PdfTrailerTreeNode trailer = controller.getPdfTree().getRoot();
            PdfObjectTreeNode catalog = factory.getChildNode(trailer, PdfName.ROOT);
            PdfObjectTreeNode form = factory.getChildNode(catalog, PdfName.ACROFORM);
            if (form == null) {
                return;
            }
            PdfObjectTreeNode fields = factory.getChildNode(form, PdfName.FIELDS);
            FormTreeNode root = new FormTreeNode();
            if (fields != null) {
                FormTreeNode node = new FormTreeNode(fields);
                node.setUserObject("Fields");
                loadFields(factory, node, fields);
                root.add(node);
            }
            PdfObjectTreeNode xfa = factory.getChildNode(form, PdfName.XFA);
            if (xfa != null) {
                XfaTreeNode node = new XfaTreeNode(xfa);
                node.setUserObject("XFA");
                loadXfa(factory, node, xfa);
                root.add(node);
                try {
                    xfaFile = new XfaFile(node);
                    xfaTree.load(xfaFile);
                    xfaTextArea.load(xfaFile);
                } catch (IOException e) {
                    e.printStackTrace();
                } catch (DocumentException e) {
                    e.printStackTrace();
                }
            }
            setModel(new DefaultTreeModel(root));
        }
    }

    
    private void loadFields(TreeNodeFactory factory, FormTreeNode form_node, PdfObjectTreeNode object_node) {
        if (object_node == null)
            return;
        factory.expandNode(object_node);
        if (object_node.isIndirectReference()) {
            loadFields(factory, form_node, (PdfObjectTreeNode)object_node.getFirstChild());
        }
        else if (object_node.isArray()) {
            Enumeration<PdfObjectTreeNode> children = object_node.children();
            while (children.hasMoreElements()) {
                loadFields(factory, form_node, children.nextElement());
            }
        }
        else if (object_node.isDictionary()) {
            FormTreeNode leaf = new FormTreeNode(object_node);
            form_node.add(leaf);
            PdfObjectTreeNode kids = factory.getChildNode(object_node, PdfName.KIDS);
            loadFields(factory, leaf, kids);
        }
    }

    
    private void loadXfa(TreeNodeFactory factory, XfaTreeNode form_node, PdfObjectTreeNode object_node) {
        if (object_node == null)
            return;
        factory.expandNode(object_node);
        if (object_node.isIndirectReference()) {
            loadXfa(factory, form_node, (PdfObjectTreeNode)object_node.getFirstChild());
        }
        else if (object_node.isArray()) {
            Enumeration<PdfObjectTreeNode> children = object_node.children();
            PdfObjectTreeNode key;
            PdfObjectTreeNode value;
            while (children.hasMoreElements()) {
                key = children.nextElement();
                value = children.nextElement();
                if (value.isIndirectReference()) {
                    factory.expandNode(value);
                    value = (PdfObjectTreeNode)value.getFirstChild();
                }
                form_node.addPacket(key.getPdfObject().toString(), value);
            }
        }
        else if (object_node.isStream()) {
            form_node.addPacket("xdp", object_node);
        }
    }

    
    public void valueChanged(TreeSelectionEvent evt) {
        if (controller == null)
            return;
        FormTreeNode selectednode = (FormTreeNode)this.getLastSelectedPathComponent();
        if (selectednode == null)
            return;
        PdfObjectTreeNode node = selectednode.getCorrespondingPdfObjectNode();
        if (node != null)
            controller.selectNode(node);
    }

    public XfaTree getXfaTree() {
        return xfaTree;
    }
    
    public XfaTextArea getXfaTextArea() {
        return xfaTextArea;
    }
    
    
    private static final long serialVersionUID = -3584003547303700407L;

}
