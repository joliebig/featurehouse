

package com.lowagie.rups;

import java.awt.Dimension;
import java.awt.Toolkit;

import javax.swing.JFrame;

import com.lowagie.rups.controller.RupsController;


public class Rups {

    
    public static final String VERSION = "RUPS 0.0.1 (by lowagie.com)";
    
    
    
    public static void main(String[] args) {
        startApplication();
    }
    
    
    
    
    public static void startApplication() {
        JFrame frame = new JFrame();
        
        Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
        frame.setSize((int)(screen.getWidth() * .90), (int)(screen.getHeight() * .90));
        frame.setLocation((int)(screen.getWidth() * .05), (int)(screen.getHeight() * .05));
        frame.setResizable(true);
        
        
        frame.setTitle("iText RUPS");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        
        RupsController controller = new RupsController(frame.getSize());
        frame.setJMenuBar(controller.getMenuBar());
        frame.getContentPane().add(controller.getMasterComponent(), java.awt.BorderLayout.CENTER);
        frame.setVisible(true);
    }
    
    
    
    
    private static final long serialVersionUID = 4386633640535735848L;

}