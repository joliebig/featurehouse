

package com.lowagie.rups.model;

import java.util.Observable;

import com.lowagie.text.pdf.PdfReader;


public class ObjectLoader extends BackgroundTask {
    
    protected Observable observable;
    
    protected PdfReader reader;
    
    protected IndirectObjectFactory objects;
    
    protected TreeNodeFactory nodes;
    
    
    public ObjectLoader(Observable observable, PdfReader reader) {
        this.observable = observable;
        this.reader = reader;
        start();
    }
    
    
    public PdfReader getReader() {
        return reader;
    }

    
    public IndirectObjectFactory getObjects() {
        return objects;
    }

    
    public TreeNodeFactory getNodes() {
        return nodes;
    }
    
    
    @Override
    public void doTask() {
        ProgressDialog progress = new ProgressDialog(null, "Reading PDF file");
        objects = new IndirectObjectFactory(reader);
        int n = objects.getXRefMaximum();
        progress.setMessage("Reading the Cross-Reference table");
        progress.setTotal(n);
        while (objects.storeNextObject()) {
            progress.setValue(objects.getCurrent());
        }
        progress.setTotal(0);
        nodes = new TreeNodeFactory(objects);
        progress.setMessage("Updating GUI");
        observable.notifyObservers(this);
        progress.dispose();
    }
}