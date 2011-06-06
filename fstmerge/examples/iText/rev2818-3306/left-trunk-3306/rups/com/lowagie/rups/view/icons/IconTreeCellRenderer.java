

package com.lowagie.rups.view.icons;

import java.awt.Component;

import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;



public class IconTreeCellRenderer extends DefaultTreeCellRenderer {

    
    private static final long serialVersionUID = 6513462839504342074L;

    
    public Component getTreeCellRendererComponent(JTree tree, Object value,
            boolean selected, boolean expanded, boolean leaf, int row,
            boolean hasFocus) {
        super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus);
        if (value instanceof IconTreeNode) {
            IconTreeNode node = (IconTreeNode) value;
            setIcon(node.getIcon());
        }
        return this;
    }
}