

package com.lowagie.rups.model;

import com.sun.pdfview.PDFFile;
import com.sun.pdfview.PDFPage;


public class PageLoader extends BackgroundTask {

    
    protected PDFFile file;
    
    protected int numberOfPages;
    
    protected boolean[] busy;
    
    protected boolean[] done;
    
    
    public PageLoader(PDFFile file) {
        super();
        this.file = file;
        numberOfPages = file.getNumPages();
        busy = new boolean[numberOfPages];
        done = new boolean[numberOfPages];
        start();
    }

    
    public int getNumberOfPages() {
        return numberOfPages;
    }

    
    public synchronized PDFPage loadPage(int pageNumber) {
        pageNumber--;
        if (busy[pageNumber]) return null;
        busy[pageNumber] = true;
        PDFPage page = file.getPage(pageNumber + 1, true);
        if (!done[pageNumber]) {
            System.out.println("Loading page " + (pageNumber + 1));
        }
        done[pageNumber] = true;
        busy[pageNumber] = false;
        return page;
    }

    
    @Override
    public void doTask() {
        for (int i = 0; i < numberOfPages; i++) {
            loadPage(i + 1);
        }
    }
}
