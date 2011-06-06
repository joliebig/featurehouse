

package com.lowagie.rups.io;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Observable;


public class FileCloseAction implements ActionListener {
    
    
    protected Observable observable;
    
    
    public FileCloseAction(Observable observable) {
        this.observable = observable;
    }
    
    
    public void actionPerformed(ActionEvent evt) {
        observable.notifyObservers(this);
    }

}
