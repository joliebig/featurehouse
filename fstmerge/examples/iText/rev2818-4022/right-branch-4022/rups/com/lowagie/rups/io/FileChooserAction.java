

package com.lowagie.rups.io;

import java.awt.event.ActionEvent;
import java.io.File;
import java.util.Observable;

import javax.swing.AbstractAction;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;


public class FileChooserAction extends AbstractAction {
    
    
    protected Observable observable;
    
    protected FileFilter filter;
    
    protected boolean newFile;
    
    protected File file;
    
    
    public FileChooserAction(Observable observable, String caption, FileFilter filter, boolean newFile) {
        super(caption);
        this.observable = observable;
        this.filter = filter;
        this.newFile = newFile;
    }
    
    
    public File getFile() {
        return file;
    }

    
    public void actionPerformed(ActionEvent evt) {
        JFileChooser fc = new JFileChooser();
        if (filter != null) {
            fc.setFileFilter(filter);
        }
        int okCancel;
        if (newFile) {
            okCancel = fc.showSaveDialog(null);
        }
        else {
            okCancel = fc.showOpenDialog(null);
        }
        if (okCancel == JFileChooser.APPROVE_OPTION) {
            file = fc.getSelectedFile();
            observable.notifyObservers(this);
        }
    }

    
    private static final long serialVersionUID = 2225830878098387118L;

}
