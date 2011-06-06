

package com.lowagie.rups.view;


public interface PageNavigationListener {

    
    public int getTotalNumberOfPages();
    
    public int getCurrentPageNumber();
    
    public int gotoFirstPage();
    
    public int gotoPreviousPage();
    
    public int gotoPage(int pageNumber);
    
    public int gotoNextPage();
    
    public int gotoLastPage();
}
