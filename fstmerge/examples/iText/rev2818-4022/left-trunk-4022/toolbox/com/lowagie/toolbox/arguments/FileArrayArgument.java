


package com.lowagie.toolbox.arguments;

import java.awt.event.ActionEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.io.File;

import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.swing.FileList;



public class FileArrayArgument extends AbstractArgument {
    public FileArrayArgument() {
        super();
        try {
            jbInit();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public FileArrayArgument(AbstractTool tool, String name, String description) {
        super(tool, name, description, null);
        try {
            jbInit();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public void actionPerformed(ActionEvent e) {
        fileList1.setLocation(10, 10);
        fileList1.setVisible(true);
        this.getTool().getInternalFrame().getDesktopPane().add(fileList1);
        try {
            fileList1.setSelected(true);
        } catch (PropertyVetoException ex1) {
            System.out.println(ex1.getMessage());
        }





    }


    public Object getArgument() throws InstantiationException {
        if (value == null) {
            return null;
        }
        try {
            return value;
        } catch (Exception e) {
            throw new InstantiationException(e.getMessage());
        }
    }

    public static void main(String[] args) {
        FileArrayArgument filearrayargument = new FileArrayArgument();
    }

    private void jbInit() throws Exception {
        fileList1.addPropertyChangeListener(this);
    }

    FileList fileList1 = new FileList();
    public void propertyChange(PropertyChangeEvent evt) {
        String propertyname = evt.getPropertyName();
        if (propertyname.equals("filevector")) {
            File[] filear = (File[]) evt.getNewValue();
            if (filear != null) {
                this.setValue(filear);
            }
        }
    }
    
    public String toString() {

        return fileList1.getStringreprasentation();
    }

}
