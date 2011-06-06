

package com.lowagie.rups.view.icons;

import javax.swing.JButton;


public class IconButton extends JButton {

    
    public IconButton(IconActionListener listener) {
        super(listener.getIcon());
        addActionListener(listener);
    }
    
    
    private static final long serialVersionUID = 3523016393512117003L;
}
