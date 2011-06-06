

package com.lowagie.rups.view;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JOptionPane;

import com.lowagie.rups.Rups;
import com.lowagie.text.Document;

public class MessageAction implements ActionListener {
    
    public void actionPerformed(ActionEvent evt) {
        String message = "Unspecified message";
        if (RupsMenuBar.ABOUT.equals(evt.getActionCommand())) {
            message = "RUPS is a tool by lowagie.com.\nIt uses iText, a Free Java-PDF Library\nand SUN's PDF Renderer.\nVisit http://www.lowagie.com/iText/ for more info.";
        }
        else if (RupsMenuBar.VERSIONS.equals(evt.getActionCommand())) {
            message = "RUPS version String: " + Rups.VERSION
            + "\niText version String: " + Document.getVersion()
            + "\nSUN's PDF Renderer version: version unknown";
        }
        JOptionPane.showMessageDialog(null, message);
    }

}
