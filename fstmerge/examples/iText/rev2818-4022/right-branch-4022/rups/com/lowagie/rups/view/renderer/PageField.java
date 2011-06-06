

package com.lowagie.rups.view.renderer;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JTextField;

import com.lowagie.rups.view.PageNavigationListener;


public class PageField extends JTextField implements ActionListener {

    
    protected PageNavigationListener listener;

    
    public PageField(PageNavigationListener listener) {
        super("", 5);
        this.listener = listener;
        setMaximumSize(new Dimension(45, 32));
        addActionListener(this);
    }
    
    
    public void actionPerformed(ActionEvent evt) {
        int pageNumber;
        try {
            pageNumber = Integer.parseInt(getText());
        } catch (NumberFormatException nfe) {
            pageNumber = -1;
        }
        listener.gotoPage(pageNumber);
    }
    
    
    private static final long serialVersionUID = 8212776141917597892L;

}
