

package com.lowagie.rups.controller;

import java.awt.Component;
import java.awt.Dimension;
import java.io.File;
import java.io.IOException;
import java.util.Observable;

import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

import com.lowagie.rups.io.FileChooserAction;
import com.lowagie.rups.io.FileCloseAction;
import com.lowagie.rups.model.PdfFile;
import com.lowagie.rups.view.Console;
import com.lowagie.rups.view.PageNavigationListener;
import com.lowagie.rups.view.RupsMenuBar;
import com.lowagie.rups.view.Utilities;
import com.lowagie.rups.view.itext.treenodes.PdfObjectTreeNode;
import com.lowagie.rups.view.itext.treenodes.PdfTrailerTreeNode;
import com.lowagie.text.DocumentException;


public class RupsController extends Observable
    implements TreeSelectionListener, PageNavigationListener {
    
    
    
    
    
    protected PdfFile pdfFile;

    
    
    protected RupsMenuBar menuBar;
    
    protected JSplitPane masterComponent;
    
    
    
    protected PdfRendererController renderer;
    
    protected PdfReaderController reader;
    
    
    
    public RupsController(Dimension dimension) {
        
        menuBar = new RupsMenuBar(this);
        addObserver(menuBar);
        Console console = Console.getInstance();
        addObserver(console);
        renderer = new PdfRendererController(this);
        addObserver(renderer);
        reader = new PdfReaderController(this, this);
        addObserver(reader);

        
        masterComponent = new JSplitPane();
        masterComponent.setOrientation(JSplitPane.VERTICAL_SPLIT);
        masterComponent.setDividerLocation((int)(dimension.getHeight() * .70));
        masterComponent.setDividerSize(2);
        
        JSplitPane content = new JSplitPane();
        masterComponent.add(content, JSplitPane.TOP);
        JSplitPane info = new JSplitPane();
        masterComponent.add(info, JSplitPane.BOTTOM);
        
        content.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
        content.setDividerLocation((int)(dimension.getWidth() * .75));
        content.setDividerSize(1);
        
        JSplitPane viewers = new JSplitPane();
        content.add(viewers, JSplitPane.LEFT);
        content.add(reader.getNavigationTabs(), JSplitPane.RIGHT);

        viewers.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
        viewers.setDividerLocation((int)(dimension.getWidth() * .35));
        viewers.setDividerSize(1);
        viewers.add(renderer, JSplitPane.RIGHT);
        viewers.add(Utilities.getScrollPane(reader.getPdfTree()), JSplitPane.LEFT);
        
        info.setDividerLocation((int) (dimension.getWidth() * .3));
        info.setDividerSize(1);
        info.add(reader.getObjectPanel(), JSplitPane.LEFT);
        JTabbedPane editorPane = reader.getEditorTabs();
        JScrollPane cons = Utilities.getScrollPane(console.getTextArea());
        editorPane.addTab("Console", null, cons, "Console window (System.out/System.err)");
        editorPane.setSelectedComponent(cons);
        info.add(editorPane, JSplitPane.RIGHT);
        
    }

    
    public RupsMenuBar getMenuBar() {
        return menuBar;
    }
    
    
    public Component getMasterComponent() {
        return masterComponent;
    }

    
    
    
    @Override
    public void notifyObservers(Object obj) {
        if (obj instanceof FileChooserAction) {
            File file = ((FileChooserAction)obj).getFile();
            try {
                pdfFile = new PdfFile(file);
                setChanged();
                super.notifyObservers(RupsMenuBar.OPEN);
                renderer.startPageLoader(pdfFile);
                reader.startObjectLoader(pdfFile);
            }
            catch(IOException ioe) {
                JOptionPane.showMessageDialog(masterComponent, ioe.getMessage(), "Dialog", JOptionPane.ERROR_MESSAGE);
            }
            catch (DocumentException de) {
                JOptionPane.showMessageDialog(masterComponent, de.getMessage(), "Dialog", JOptionPane.ERROR_MESSAGE);
            }
            return;
        }
        if (obj instanceof FileCloseAction) {
            pdfFile = null;
            setChanged();
            super.notifyObservers(RupsMenuBar.CLOSE);
            return;
        }
    }

    
    
    
    public void valueChanged(TreeSelectionEvent evt) {
        Object selectednode = reader.getPdfTree().getLastSelectedPathComponent();
        if (selectednode instanceof PdfTrailerTreeNode) {
            menuBar.update(this, RupsMenuBar.FILE_MENU);
            return;
        }
        if (selectednode instanceof PdfObjectTreeNode) {
            reader.update(this, selectednode);
        }
    }

    
    
    
    public int getCurrentPageNumber() {
        return renderer.getCurrentPageNumber();
    }

    
    public int getTotalNumberOfPages() {
        return renderer.getTotalNumberOfPages();
    }
    
    
    public int gotoFirstPage() {
        return gotoPage(1);
    }

    
    public int gotoPreviousPage() {
        return gotoPage(getCurrentPageNumber() - 1);
    }

    
    public int gotoPage(int pageNumber) {
        pageNumber = renderer.gotoPage(pageNumber);
        reader.gotoPage(pageNumber);
        return pageNumber;
    }
    
    
    public int gotoNextPage() {
        return gotoPage(getCurrentPageNumber() + 1);
    }

    
    public int gotoLastPage() {
        return gotoPage(getTotalNumberOfPages());
    }
}
