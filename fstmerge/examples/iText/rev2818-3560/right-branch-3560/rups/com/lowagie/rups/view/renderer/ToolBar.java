

package com.lowagie.rups.view.renderer;

import javax.swing.Box;
import javax.swing.JToolBar;

import com.lowagie.rups.view.PageNavigationAction;
import com.lowagie.rups.view.PageNavigationListener;
import com.lowagie.rups.view.icons.IconButton;


public class ToolBar extends JToolBar {

    
    protected PageField pageField;
    
    
    public ToolBar(PageNavigationListener listener) {
        super();
        pageField = new PageField(listener);
        
        setFloatable(false);
        add(Box.createHorizontalGlue());
        addNavigationButton(listener, PageNavigationAction.FIRST_PAGE);
        addNavigationButton(listener, PageNavigationAction.PREVIOUS_PAGE);
        add(pageField);
        addNavigationButton(listener, PageNavigationAction.NEXT_PAGE);
        addNavigationButton(listener, PageNavigationAction.LAST_PAGE);
    }
    
    
    public void setPageNumber(int pageNumber) {
        if (pageNumber > 0) {
            pageField.setText(String.valueOf(pageNumber));
        }
        else {
            pageField.setText("");
        }
    }
    
    
    protected void addNavigationButton(PageNavigationListener listener, int type) {
        add(new IconButton(new PageNavigationAction(listener, type, true)));
    }
    
    
    private static final long serialVersionUID = -2062747250230455558L;

}