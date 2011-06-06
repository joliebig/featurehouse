

package com.lowagie.rups.view.icons;

import java.util.HashMap;

import javax.swing.Icon;
import javax.swing.ImageIcon;


public class IconFetcher {
    
    
    protected static HashMap<String, Icon> cache = new HashMap<String, Icon>();
    
    
    public static Icon getIcon(String filename) {
        if (filename == null) {
            return null;
        }
        Icon icon = cache.get(filename);
        if (icon == null) {
            try {
                icon = new ImageIcon(IconFetcher.class.getResource(filename));
                cache.put(filename, icon);
            }
            catch(Exception e) {
                System.err.println("Can't find file: " + filename);
                return null;
            }
        }
        return icon;
    }
}
