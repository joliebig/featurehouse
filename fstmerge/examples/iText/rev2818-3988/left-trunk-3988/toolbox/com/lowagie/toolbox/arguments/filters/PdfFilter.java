



package com.lowagie.toolbox.arguments.filters;

import java.io.File;

import javax.swing.filechooser.FileFilter;


public class PdfFilter extends FileFilter {

    
    public boolean accept(File f) {
        if (f.isDirectory()) return true;
        if (f.getName().toLowerCase().endsWith(".pdf")) return true;
        return false;
    }

    
    public String getDescription() {
        return "*.pdf PDF files";
    }

}
