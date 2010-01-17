package net.sf.jabref.gui;

import java.awt.event.*;
import java.io.IOException;
import java.util.Comparator;

import javax.swing.JPopupMenu;
import javax.swing.SwingUtilities;
import javax.swing.Timer;

import net.sf.jabref.*;
import net.sf.jabref.external.ExternalFileMenuItem;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.SortedList;
import ca.odell.glazedlists.event.ListEvent;
import ca.odell.glazedlists.event.ListEventListener;


public class MainTableSelectionListener implements ListEventListener<BibtexEntry>, MouseListener,
        KeyListener, FocusListener {

    PreviewPanel[] previewPanel = null;
    int activePreview = 1;
    PreviewPanel preview;
    MainTable table;
    BasePanel panel;
    EventList<BibtexEntry> tableRows;
    private boolean previewActive = Globals.prefs.getBoolean("previewEnabled");
    private boolean workingOnPreview = false;

    
    
    
    private int[] lastPressed = new int[20];
    private int lastPressedCount = 0;
    private long lastPressedTime = 0;
    private long QUICK_JUMP_TIMEOUT = 2000;

    

    public MainTableSelectionListener(BasePanel panel, MainTable table) {
        this.table = table;
        this.panel = panel;
        this.tableRows = table.getTableRows();
        instantiatePreviews();
        this.preview = previewPanel[activePreview];
    }

    private void instantiatePreviews() {
        previewPanel = new PreviewPanel[]
                {new PreviewPanel(panel.database(), panel.metaData(), Globals.prefs.get("preview0")),
                        new PreviewPanel(panel.database(), panel.metaData(), Globals.prefs.get("preview1"))};
        BibtexEntry testEntry = PreviewPrefsTab.getTestEntry();
        previewPanel[0].setEntry(testEntry);
        previewPanel[1].setEntry(testEntry);
    }

    public void updatePreviews() {
        try {
            previewPanel[0].readLayout(Globals.prefs.get("preview0"));
            previewPanel[1].readLayout(Globals.prefs.get("preview1"));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void listChanged(ListEvent<BibtexEntry> e) {
        EventList<BibtexEntry> selected = e.getSourceList();
        Object newSelected = null;
        while (e.next()) {
            if (e.getType() == ListEvent.INSERT) {
                if (newSelected != null)
                    return; 
                else {
                    if (e.getIndex() < selected.size())
                        newSelected = selected.get(e.getIndex());
                }

            }
        }


        if (newSelected != null) {

            
            final BibtexEntry toShow = (BibtexEntry) newSelected;
            final int mode = panel.getMode(); 
            if ((mode == BasePanel.WILL_SHOW_EDITOR) || (mode == BasePanel.SHOWING_EDITOR)) {
                
                EntryEditor oldEditor = panel.getCurrentEditor();
                
                EntryEditor newEditor = panel.getEntryEditor(toShow);
                
                if ((newEditor != oldEditor) || (mode != BasePanel.SHOWING_EDITOR)) {
                    panel.showEntryEditor(newEditor);
                }
            } else {
                
                if (previewActive) {
                    updatePreview(toShow, false);
                }

            }
        }

    }

    private void updatePreview(final BibtexEntry toShow, final boolean changedPreview) {
        updatePreview(toShow, changedPreview, 0);
    }

    private void updatePreview(final BibtexEntry toShow, final boolean changedPreview, int repeats) {
        if (workingOnPreview) {
            if (repeats > 0)
                return; 
            Timer t = new Timer(50, new ActionListener() {
                public void actionPerformed(ActionEvent actionEvent) {
                    updatePreview(toShow, changedPreview, 1);
                }
            });
            t.setRepeats(false);
            t.start();
            return;
        }
        EventList<BibtexEntry> list = table.getSelected();
        
        if ((list.size() != 1) || (list.get(0) != toShow)) {
            return;
        }
        final int mode = panel.getMode();
        workingOnPreview = true;
        final Runnable update = new Runnable() {
            public void run() {
                
                if (changedPreview || (mode == BasePanel.SHOWING_NOTHING)) {
                    panel.showPreview(preview);
                    panel.adjustSplitter();
                }
                workingOnPreview = false;
            }
        };
        final Runnable worker = new Runnable() {
            public void run() {
                preview.setEntry(toShow);
                SwingUtilities.invokeLater(update);
            }
        };
        (new Thread(worker)).start();
    }

    public void editSignalled() {
        if (table.getSelected().size() == 1) {
            editSignalled((BibtexEntry) table.getSelected().get(0));
        }
    }

    public void editSignalled(BibtexEntry entry) {
        final int mode = panel.getMode();
        EntryEditor editor = panel.getEntryEditor(entry);
        if (mode != BasePanel.SHOWING_EDITOR) {
            panel.showEntryEditor(editor);
            panel.adjustSplitter();
        }
        new FocusRequester(editor);
    }

    public void mouseReleased(MouseEvent e) {
        
        final int row = table.rowAtPoint(e.getPoint());
        
        if (e.isPopupTrigger()) {
            processPopupTrigger(e, row);
            return;
        }
    }

     public void mousePressed(MouseEvent e) {
         
        
        final int col = table.columnAtPoint(e.getPoint()),
                row = table.rowAtPoint(e.getPoint());

        
        if (e.getClickCount() == 2) {

            BibtexEntry toShow = (BibtexEntry) tableRows.get(row);
            editSignalled(toShow);
        }

        
        final String[] iconType = table.getIconTypeForColumn(col);

        
        if (e.isPopupTrigger()) {
            if (iconType == null)
                processPopupTrigger(e, row);
            else
                showIconRightClickMenu(e, row, iconType);

            return;
        }


        if (iconType != null) {

            Object value = table.getValueAt(row, col);
            if (value == null) return; 

            final BibtexEntry entry = (BibtexEntry) tableRows.get(row);

            
            int hasField = -1;
            for (int i = iconType.length - 1; i >= 0; i--)
                if (entry.getField(iconType[i]) != null)
                    hasField = i;
            if (hasField == -1)
                return;
            final String fieldName = iconType[hasField];

            
            (new Thread() {
                public void run() {
                    panel.output(Globals.lang("External viewer called") + ".");

                    Object link = entry.getField(fieldName);
                    if (iconType == null) {
                        Globals.logger("Error: no link to " + fieldName + ".");
                        return; 
                    }

                    try {
                        
                        
                        if (fieldName.equals(GUIGlobals.FILE_FIELD)) {

                            
                            FileListTableModel fileList = new FileListTableModel();
                            fileList.setContent((String)link);
                            
                            if (fileList.getRowCount() > 0) {
                                FileListEntry flEntry = fileList.getEntry(0);
                                ExternalFileMenuItem item = new ExternalFileMenuItem
                                        (panel.frame(), entry, "",
                                        flEntry.getLink(), flEntry.getType().getIcon(),
                                        panel.metaData(), flEntry.getType());
                                item.actionPerformed(null);
                            }
                        } else
                            Util.openExternalViewer(panel.metaData(), (String)link, fieldName);
                    }
                    catch (IOException ex) {
                        panel.output(Globals.lang("Error") + ": " + ex.getMessage());
                    }
                }

            }).start();
        }
    }

    
    protected void processPopupTrigger(MouseEvent e, int row) {
         int selRow = table.getSelectedRow();
         if (selRow == -1 ||
                 !table.isRowSelected(table.rowAtPoint(e.getPoint()))) {
             table.setRowSelectionInterval(row, row);
             
         }
         RightClickMenu rightClickMenu = new RightClickMenu(panel, panel.metaData());
         rightClickMenu.show(table, e.getX(), e.getY());
     }

    
    private void showIconRightClickMenu(MouseEvent e, int row, String[] iconType) {
        BibtexEntry entry = (BibtexEntry) tableRows.get(row);
        JPopupMenu menu = new JPopupMenu();
        int count = 0;

        
        
        if (iconType[0].equals(GUIGlobals.FILE_FIELD)) {
            
            Object o = entry.getField(iconType[0]);
            FileListTableModel fileList = new FileListTableModel();
            fileList.setContent((String)o);
            
            for (int i=0; i<fileList.getRowCount(); i++) {
                FileListEntry flEntry = fileList.getEntry(i);
                String description = flEntry.getDescription();
                if ((description == null) || (description.trim().length() == 0))
                    description = flEntry.getLink();
                menu.add(new ExternalFileMenuItem(panel.frame(), entry, description,
                        flEntry.getLink(), flEntry.getType().getIcon(), panel.metaData(),
                        flEntry.getType()));
                count++;
            }

        }
        else {
            for (int i=0; i<iconType.length; i++) {
                Object o = entry.getField(iconType[i]);
                if (o != null) {
                    menu.add(new ExternalFileMenuItem(panel.frame(), entry, (String)o, (String)o,
                            GUIGlobals.getTableIcon(iconType[i]).getIcon(),
                            panel.metaData()));
                    count++;
                }
            }
        }
        if (count == 0) {
            processPopupTrigger(e, row);
            return;
        }
        menu.show(table, e.getX(), e.getY());
    }

    public void entryEditorClosing(EntryEditor editor) {
        preview.setEntry(editor.getEntry());
        if (previewActive)
            panel.showPreview(preview);
        else
            panel.hideBottomComponent();
        panel.adjustSplitter();
        new FocusRequester(table);
    }


    public void mouseClicked(MouseEvent e) {

    }

    public void mouseEntered(MouseEvent e) {

    }

    public void mouseExited(MouseEvent e) {

    }

    public void setPreviewActive(boolean enabled) {
        previewActive = enabled;
        if (!previewActive) {
            panel.hideBottomComponent();
        } else {
            if (table.getSelected().size() > 0 ) {
                updatePreview((BibtexEntry) table.getSelected().get(0), false);
            }
        }
    }

    public void switchPreview() {
        if (activePreview < previewPanel.length - 1)
            activePreview++;
        else
            activePreview = 0;
        if (previewActive) {
            this.preview = previewPanel[activePreview];

            if (table.getSelected().size() > 0) {
                updatePreview((BibtexEntry) table.getSelected().get(0), true);
            }
        }
    }

    
    public void keyTyped_(KeyEvent e) {
        
        if ((!e.isActionKey()) && Character.isLetterOrDigit(e.getKeyChar())) {
            int sortingColumn = table.getSortingColumn(0);
            if (sortingColumn < 0)
                return;
            Comparator<BibtexEntry> comp = table.getComparatorForColumn(sortingColumn);
            int piv = 1;
            while (((sortingColumn = table.getSortingColumn(piv)) >= 0)
                && ((comp = table.getComparatorForColumn(sortingColumn)) != null)
                && !(comp instanceof FieldComparator)) {
                piv++;
            }
            if ((comp == null) || !(comp instanceof FieldComparator))
                return;

            
            String field = ((FieldComparator)comp).getFieldName();
            System.out.println(String.valueOf(e.getKeyChar())+" "+field);
            SortedList<BibtexEntry> list = table.getSortedForTable();
            BibtexEntry testEntry = new BibtexEntry("0");
            testEntry.setField(field, String.valueOf(e.getKeyChar()));
            int i = list.sortIndex(testEntry);
            System.out.println(i);
        }
    }

    
    public void keyTyped(KeyEvent e) {
        if ((!e.isActionKey()) && Character.isLetterOrDigit(e.getKeyChar())
	    
	    && (e.getModifiers() == 0)) {
            long time = System.currentTimeMillis();
            if (time - lastPressedTime > QUICK_JUMP_TIMEOUT)
                lastPressedCount = 0; 
            
            lastPressedTime = time;
            
            int c = e.getKeyChar();
            if (lastPressedCount < lastPressed.length)
                lastPressed[lastPressedCount++] = c;

            int sortingColumn = table.getSortingColumn(0);
            if (sortingColumn == -1)
                return; 
            
            
            
            int startRow = 0;
            

            boolean done = false;
            while (!done) {
                for (int i=startRow; i<table.getRowCount(); i++) {
                    Object o = table.getValueAt(i, sortingColumn);
                    if (o == null)
                        continue;
                    String s = o.toString().toLowerCase();
                    if (s.length() >= lastPressedCount)
                        for (int j=0; j<lastPressedCount; j++) {
                            if (s.charAt(j) != lastPressed[j])
                                break; 
                            else if (j == lastPressedCount-1) {
                                
                                table.setRowSelectionInterval(i, i);
                                table.ensureVisible(i);
                                return;
                            }
                        }
                    
                    
                }
                
                
                if (startRow > 0)
                    startRow = 0;
                else
                    done = true;

            }
            
        } else if (e.getKeyChar() == KeyEvent.VK_ESCAPE) {
            lastPressedCount = 0;

        }
    }

    public void keyReleased(KeyEvent e) {
    }

    public void keyPressed(KeyEvent e) {
    }

    public void focusGained(FocusEvent e) {

    }

    public void focusLost(FocusEvent e) {
        lastPressedCount = 0; 
    }
}
