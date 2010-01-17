package net.sf.jabref; 

import java.awt.Color; 

import javax.swing.Icon; 
import javax.swing.JLabel; 
import javax.swing.table.DefaultTableCellRenderer; 

import java.awt.*; 

import javax.swing.*; 


public  class  GeneralRenderer   extends DefaultTableCellRenderer {
	

    public GeneralRenderer(Color c) {
        super();
        this.background = c;
        setBackground(c);
    }


	

    <<<<<<< C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var1_40948
public GeneralRenderer(Color c) {
        super();
        this.background = c;
        setBackground(c);
=======
public GeneralRenderer(Color c, Color fg) {
        this(c);
        setForeground(fg);
>>>>>>> C:\Dokumente und Einstellungen\Sven Apel\Eigene Dateien\Uni\fstmerge\fstmerge_tmp\fstmerge_var2_40950
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


	

    Color background, selBackground = null;

	

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


}
