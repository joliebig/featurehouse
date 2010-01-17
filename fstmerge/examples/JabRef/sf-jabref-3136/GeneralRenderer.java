package net.sf.jabref;

import java.awt.*;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;


public class GeneralRenderer  extends DefaultTableCellRenderer {

    Color background, selBackground = null;

    public GeneralRenderer(Color c) {
        super();
        this.background = c;
        setBackground(c);
    }


    
    public GeneralRenderer(Color c, Color fg) {
        this(c);
        this.background = c;
        setForeground(fg);
    }

    
    public GeneralRenderer(Color c, Color fg, Color sel) {
        this(c);
        this.background = c;
        setForeground(fg);
        this.selBackground = sel;
    }

    public Component getTableCellRendererComponent(JTable table, Object o, boolean isSelected,
                                                   boolean hasFocus, int row, int column) {
        if (selBackground == null)
            return super.getTableCellRendererComponent(table, o, isSelected, hasFocus, row, column);
        else {
            Component c = super.getTableCellRendererComponent(table, o, isSelected, hasFocus, row, column);
            if (isSelected)
                c.setBackground(selBackground);
            else
                c.setBackground(background);
            return c;
        }
    }

    public void firePropertyChange(String propertyName, boolean old, boolean newV) {}
    public void firePropertyChange(String propertyName, Object old, Object newV) {}

    
    protected void setValue(Object value) {
        
        if (value instanceof Icon) {
            setIcon((Icon)value);
            setText(null);
            
        } else if (value instanceof JLabel) {
          JLabel lab = (JLabel)value;
          setIcon(lab.getIcon());
          
          setToolTipText(lab.getToolTipText());
          if (lab.getIcon() != null)
            setText(null);
        } else {

            setIcon(null);
            
            setToolTipText(null);
            if (value != null)
                setText(value.toString());
            else
                setText(null);
        }
    }

    

}
