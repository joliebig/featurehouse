

package com.lowagie.rups.controller;

import java.util.Observable;
import java.util.Observer;

import javax.swing.JSplitPane;

import com.lowagie.rups.model.PageLoader;
import com.lowagie.rups.model.PdfFile;
import com.lowagie.rups.view.PageNavigationListener;
import com.lowagie.rups.view.RupsMenuBar;
import com.lowagie.rups.view.renderer.PagePanel;
import com.lowagie.rups.view.renderer.ToolBar;


public class PdfRendererController extends JSplitPane
    implements Observer {
    
    
    protected PageLoader pageLoader = null;
    
    protected ToolBar toolbar;
    
    protected PagePanel pagePanel;
    
    
    private static final long serialVersionUID = 3270054619281094248L;

    
    public PdfRendererController(PageNavigationListener listener) {
        setOrientation(JSplitPane.VERTICAL_SPLIT);
        setDividerLocation(33);
        setDividerSize(0);
        pagePanel = new PagePanel();
        toolbar = new ToolBar(listener);
        add(toolbar, JSplitPane.TOP);
        add(pagePanel, JSplitPane.BOTTOM);
    }
    
    
    public void startPageLoader(PdfFile file) {
        this.pageLoader = new PageLoader(file.getPDFFile());
        gotoPage(1);
    }
    
    
    protected int showPage(int pageNumber) {
        if (pageLoader == null) {
            return -1;
        }
        pagePanel.showPage(pageLoader.loadPage(pageNumber));
        pagePanel.requestFocus();
        return pageNumber;
    }

    
    
    
    public int getTotalNumberOfPages() {
        if (pageLoader == null) return 0;
        return pageLoader.getNumberOfPages();
    }
    
    
    public int getCurrentPageNumber() {
        return pagePanel.getCurrentPageNumber();
    }
    
    
    public int gotoPage(int pageNumber) {
        if (pageNumber == getCurrentPageNumber()) {
            return pageNumber;
        }
        if (pageNumber < 0) {
            toolbar.setPageNumber(-1);
            return -1;
        }
        if (pageNumber == 0) {
            pageNumber = 1;
        }
        else if (pageNumber > getTotalNumberOfPages()) {
            pageNumber = getTotalNumberOfPages();
        }
        pageNumber = showPage(pageNumber);
        toolbar.setPageNumber(pageNumber);
        return pageNumber;
    }
    
    
    
    
    public void update(Observable observable, Object obj) {
        if (RupsMenuBar.CLOSE.equals(obj)) {
            pageLoader = null;
            pagePanel.showPage(null);
            toolbar.setPageNumber(-1);
        }
    }
}
