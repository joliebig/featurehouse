

package com.lowagie.rups.view;

import java.awt.Component;

import javax.swing.JScrollPane;


public class Utilities {
    
    public static JScrollPane getScrollPane(Component component) {
        JScrollPane scrollpane = new JScrollPane();
        scrollpane.setViewportView(component);
        return scrollpane;
    }
}
