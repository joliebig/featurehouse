



package com.lowagie.toolbox.arguments.filters;

import java.io.File;

import javax.swing.filechooser.FileFilter;


public class ImageFilter extends FileFilter {

    
    public static final String[] IMAGES = new String[8];
    static {
        IMAGES[0] = ".jpg";
        IMAGES[1] = ".jpeg";
        IMAGES[2] = ".png";
        IMAGES[3] = ".gif";
        IMAGES[4] = ".bmp";
        IMAGES[5] = ".wmf";
        IMAGES[6] = ".tif";
        IMAGES[7] = ".tiff";
    }

    
    public boolean[] filter = new boolean[8];

    
    public ImageFilter() {
        for (int i = 0; i < filter.length; i++) {
            filter[i] = true;
        }
    }

    
    public ImageFilter(boolean jpeg, boolean png, boolean gif, boolean bmp, boolean wmf, boolean tiff) {
        if (jpeg) {
            filter[0] = true;
            filter[1] = true;
        }
        if (png) {
            filter[2] = true;
        }
        if (gif) {
            filter[3] = true;
        }
        if (bmp) {
            filter[4] = true;
        }
        if (wmf) {
            filter[5] = true;
        }
        if (tiff) {
            filter[6] = true;
            filter[7] = true;
        }
    }

    
    public boolean accept(File f) {
        if (f.isDirectory()) return true;
        for (int i = 0; i < IMAGES.length; i++) {
            if (filter[i] && f.getName().toLowerCase().endsWith(IMAGES[i])) return true;
        }
        return false;
    }

    
    public String getDescription() {
        return "Image files";
    }
}
