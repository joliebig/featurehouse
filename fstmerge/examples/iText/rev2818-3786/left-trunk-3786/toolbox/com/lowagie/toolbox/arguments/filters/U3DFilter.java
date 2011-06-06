



package com.lowagie.toolbox.arguments.filters;

import java.io.File;

import javax.swing.filechooser.FileFilter;


public class U3DFilter extends FileFilter {

    
    public boolean accept(File f) {
        if (f.isDirectory()) return true;
        if (f.getName().toLowerCase().endsWith(".u3d")) return true;
        return false;
    }

    
    public String getDescription() {
        return "*.u3d U3D files";
    }

}
