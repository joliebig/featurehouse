
package com.lowagie.tools;

import java.awt.GraphicsEnvironment;
import java.lang.reflect.Method;

import javax.swing.JOptionPane;

import com.lowagie.text.Document;

public class ToolboxAvailable {

    
    public static void main(String[] args) {
        if (GraphicsEnvironment.isHeadless()) {
            System.out.println(Document.getVersion() + " Toolbox error: headless display");
        } else
        try {
            Class c = Class.forName("com.lowagie.tools.Toolbox");
            Method toolboxMain = c.getMethod("main", new Class[] {args.getClass()});
            toolboxMain.invoke(null, new Object[] {args} );
        } catch (Exception e) {
            JOptionPane.showMessageDialog(null,
                    "You need the toolbox.jar with class com.lowagie.tools.Toolbox to use the iText Toolbox.",
                    Document.getVersion() + " Toolbox error",
                    JOptionPane.ERROR_MESSAGE);
        }
    }
}