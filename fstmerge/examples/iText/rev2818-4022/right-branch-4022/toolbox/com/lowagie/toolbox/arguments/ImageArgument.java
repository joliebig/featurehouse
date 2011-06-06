



package com.lowagie.toolbox.arguments;

import java.awt.event.ActionEvent;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

import com.lowagie.text.Image;
import com.lowagie.toolbox.AbstractTool;
import com.lowagie.toolbox.arguments.filters.ImageFilter;


public class ImageArgument extends AbstractArgument {
    
    private FileFilter filter;

    
    public ImageArgument(AbstractTool tool, String name, String description,
                         FileFilter filter) {
        super(tool, name, description, null);
        this.filter = filter;
    }

    
    public ImageArgument(AbstractTool tool, String name, String description) {
        this(tool, name, description, new ImageFilter());
    }

    
    public Object getArgument() throws InstantiationException {
        if (value == null) {
            return null;
        }
        try {
            return Image.getInstance(value.toString());
        } catch (Exception e) {
            throw new InstantiationException(e.getMessage());
        }
    }

    
    public void actionPerformed(ActionEvent e) {
        JFileChooser fc = new JFileChooser();
        if (filter != null) {
            fc.setFileFilter(filter);
        }
        fc.showOpenDialog(tool.getInternalFrame());
        try {
            setValue(fc.getSelectedFile().getAbsolutePath());
        } catch (NullPointerException npe) {
        }
    }

}
