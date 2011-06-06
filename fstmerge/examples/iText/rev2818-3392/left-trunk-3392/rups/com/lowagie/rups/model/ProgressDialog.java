

package com.lowagie.rups.model;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.UIManager;


public class ProgressDialog extends JDialog {

    
    private static final long serialVersionUID = -8286949678008659120L;
    
    protected JLabel message;
    
    protected JProgressBar progress;
    
    public static final JLabel INFO = new JLabel(UIManager.getIcon("OptionPane.informationIcon"));
    
    
    public ProgressDialog(JFrame parent, String msg) {
        super();
        this.setTitle("Progress...");
        setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
        setSize(300, 100);
        this.setLocationRelativeTo(parent);
        
        setLayout(new GridBagLayout());
        GridBagConstraints constraints = new GridBagConstraints();
        constraints.gridx = 0;
        constraints.gridy = 0;
        constraints.gridheight = 2;
        getContentPane().add(INFO, constraints);
        constraints.gridheight = 1;
        constraints.gridx = 1;
        constraints.insets = new Insets(5, 5, 5, 5);
        message = new JLabel(msg);
        getContentPane().add(message, constraints);
        constraints.gridy = 1;
        progress = new JProgressBar();
        progress.setIndeterminate(true);
        getContentPane().add(progress, constraints);
        
        setVisible(true);
    }
    
    
    public void setMessage(String msg) {
        message.setText(msg);
    }

    
    public void setValue(int value) {
        progress.setValue(value);
    }
    
    
    public void setTotal(int n) {
        if (n > 0) {
            progress.setMaximum(n);
            progress.setIndeterminate(false);
            progress.setStringPainted(true);
        }
        else {
            progress.setIndeterminate(true);
            progress.setStringPainted(false);
        }
    }
}
