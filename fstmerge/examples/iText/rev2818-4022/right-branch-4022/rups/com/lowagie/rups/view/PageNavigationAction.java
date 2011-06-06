

package com.lowagie.rups.view;

import java.awt.event.ActionEvent;

import javax.swing.Icon;

import com.lowagie.rups.view.icons.IconActionListener;
import com.lowagie.rups.view.icons.IconFetcher;


public class PageNavigationAction implements IconActionListener {
    
    public static final int FIRST_PAGE = 1;
    
    public static final int PREVIOUS_PAGE = 2;
    
    public static final int NEXT_PAGE = 3;
    
    public static final int LAST_PAGE = 4;
    
    
    protected int type;
    
    protected PageNavigationListener listener;
    
    protected Icon icon = null;
    
    
    public PageNavigationAction(PageNavigationListener listener, int type, boolean withIcon) {
        super();
        this.listener = listener;
        this.type = type;
        if (withIcon) {
            switch(type) {
            case FIRST_PAGE:
                icon = IconFetcher.getIcon("navigation_first.png");
                break;
            case PREVIOUS_PAGE:
                icon = IconFetcher.getIcon("navigation_previous.png");
                break;
            case NEXT_PAGE:
                icon = IconFetcher.getIcon("navigation_next.png");
                break;
            case LAST_PAGE:
                icon = IconFetcher.getIcon("navigation_last.png");
                break;
            }
        }
    }

    
    public Icon getIcon() {
        return icon;
    }

    
    public void actionPerformed(ActionEvent evt) {
        switch(type) {
        case FIRST_PAGE:
            listener.gotoFirstPage();
            return;
        case PREVIOUS_PAGE:
            listener.gotoPreviousPage();
            return;
        case NEXT_PAGE:
            listener.gotoNextPage();
            return;
        case LAST_PAGE:
            listener.gotoLastPage();
            return;
        }    
    }
}
