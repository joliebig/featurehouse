
package com.lowagie.tools;

import java.lang.reflect.Method;

import javax.swing.JOptionPane;

public class ToolboxAvailable {

    
    public static void main(String[] args) {
        try {
            Class<?> c = Class.forName("com.lowagie.tools.Toolbox");
            Method toolboxMain = c.getMethod("main", new Class[] {args.getClass()});
            toolboxMain.invoke(null, new Object[] {args} );
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,
                    "You need the toolbox.jar with class com.lowagie.tools.Toolbox to use the iText Toolbox.");
        }
    }
}