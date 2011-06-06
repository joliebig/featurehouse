

package com.lowagie.rups.view;

import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Observable;
import java.util.Observer;

import javax.swing.Box;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

import com.lowagie.rups.io.FileChooserAction;
import com.lowagie.rups.io.FileCloseAction;
import com.lowagie.rups.io.filters.PdfFilter;

public class RupsMenuBar extends JMenuBar implements Observer {

    
    public static final String FILE_MENU = "File";
    
    public static final String OPEN = "Open";
    
    public static final String CLOSE = "Close";
    
    public static final String HELP_MENU = "Help";
    
    public static final String ABOUT = "About";
    
    public static final String VERSIONS = "Versions";
    
    
    protected Observable observable;
    
    protected FileChooserAction fileChooserAction;
    
    protected HashMap<String, JMenuItem> items;
    
    
    public RupsMenuBar(Observable observable) {
        this.observable = observable;
        items = new HashMap<String, JMenuItem>();
        fileChooserAction = new FileChooserAction(observable, "Open", PdfFilter.INSTANCE, false);
        MessageAction message = new MessageAction();
        JMenu file = new JMenu(FILE_MENU);
        addItem(file, OPEN, fileChooserAction);
        addItem(file, CLOSE, new FileCloseAction(observable));
        add(file);
        add(Box.createGlue());
        JMenu help = new JMenu(HELP_MENU);
        addItem(help, ABOUT, message);
        addItem(help, VERSIONS, message);
        add(help);
        enableItems(false);
    }
    
    
    public void update(Observable observable, Object obj) {
        if (OPEN.equals(obj)) {
            enableItems(true);
            return;
        }
        if (CLOSE.equals(obj)) {
            enableItems(false);
            return;
        }
        if (FILE_MENU.equals(obj)) {
            fileChooserAction.actionPerformed(null);
        }
    }
    
    
    protected void addItem(JMenu menu, String caption, ActionListener action) {
        JMenuItem item = new JMenuItem(caption);
        item.addActionListener(action);
        menu.add(item);
        items.put(caption, item);
    }
    
    
    protected void enableItems(boolean enabled) {
        enableItem(CLOSE, enabled);
    }
    
    
    protected void enableItem(String caption, boolean enabled) {
        items.get(caption).setEnabled(enabled);
    }
    
    
    private static final long serialVersionUID = 6403040037592308742L;
}