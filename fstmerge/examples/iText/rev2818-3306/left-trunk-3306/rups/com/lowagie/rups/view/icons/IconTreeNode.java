

package com.lowagie.rups.view.icons;

import javax.swing.Icon;
import javax.swing.tree.DefaultMutableTreeNode;


public class IconTreeNode extends DefaultMutableTreeNode {

    
    protected Icon icon;
    
    
    public IconTreeNode(String icon) {
        super();
        this.icon = IconFetcher.getIcon(icon);
    }
    
    
    public IconTreeNode(String icon, Object userobject) {
        super(userobject);
        this.icon = IconFetcher.getIcon(icon);
    }
    
    
    public Icon getIcon() {
        return icon;
    }

    
    private static final long serialVersionUID = -5900308991182960842L;
}