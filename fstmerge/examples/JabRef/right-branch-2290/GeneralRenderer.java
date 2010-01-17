package net.sf.jabref;

import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.*;
import java.awt.*;


public class GeneralRenderer  extends DefaultTableCellRenderer {

    public GeneralRenderer(Color c) {
        super();
        setBackground(c);
    }


    public GeneralRenderer(Color c, Color fg) {
        this(c);
        setForeground(fg);
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
