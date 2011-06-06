



package com.lowagie.toolbox.arguments;

import java.awt.event.ActionEvent;
import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.filters.DirFilter;
import com.lowagie.toolbox.swing.PdfInformationPanel;


public class FileArgument extends AbstractArgument {
    
    protected FileFilter filter;
    
    protected boolean newFile;
    
    PdfInformationPanel label = null;

    public FileArgument() {
        super();
    }

    
    public FileArgument(AbstractTool tool, String name, String description,
                        boolean newFile, FileFilter filter) {
        super(tool, name, description, null);
        this.newFile = newFile;
        this.filter = filter;
    }

    
    public FileArgument(AbstractTool tool, String name, String description,
                        boolean newFile) {
        this(tool, name, description, newFile, null);
    }

    
    public Object getArgument() throws InstantiationException {
        if (value == null) {
            return null;
        }
        try {
            return new File(value.toString());
        } catch (Exception e) {
            throw new InstantiationException(e.getMessage());
        }
    }

    
    public void actionPerformed(ActionEvent e) {
        JFileChooser fc = new JFileChooser();

        if (filter != null) {
            fc.setFileFilter(filter);
            if (filter instanceof DirFilter) {
                fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            }
        }
        if (label != null) {
            fc.setAccessory(label);
            fc.addPropertyChangeListener(
                    JFileChooser.SELECTED_FILE_CHANGED_PROPERTY, label);
        }
        if (newFile) {
            fc.showSaveDialog(tool.getInternalFrame());
        } else {
            fc.showOpenDialog(tool.getInternalFrame());
        }
        try {
            setValue(fc.getSelectedFile());
        } catch (NullPointerException npe) {
        }
    }

    
    public FileFilter getFilter() {
        return filter;
    }

    
    public void setFilter(FileFilter filter) {
        this.filter = filter;
    }

    
    public PdfInformationPanel getLabel() {
        return label;
    }

    
    public void setLabel(PdfInformationPanel label) {
        this.label = label;
    }

}
