package net.sf.jabref.gui; 

import java.awt.BorderLayout; 
import java.awt.Color; 
import java.awt.Dimension; 
import java.awt.Rectangle; 
import java.awt.event.ActionEvent; 
import java.awt.event.MouseAdapter; 
import java.awt.event.MouseEvent; 
import java.awt.event.WindowAdapter; 
import java.awt.event.WindowEvent; 
import java.io.IOException; 
import java.util.Comparator; 
import java.util.HashMap; 

import javax.swing.AbstractAction; 
import javax.swing.ActionMap; 
import javax.swing.InputMap; 
import javax.swing.JComponent; 
import javax.swing.JDialog; 
import javax.swing.JLabel; 
import javax.swing.JPopupMenu; 
import javax.swing.JScrollPane; 
import javax.swing.JTable; 
import javax.swing.SwingUtilities; 
import javax.swing.table.TableColumnModel; 

import net.sf.jabref.BasePanel; 
import net.sf.jabref.BibtexEntry; 
import net.sf.jabref.BibtexFields; 
import net.sf.jabref.EntryComparator; 
import net.sf.jabref.FieldComparator; 
import net.sf.jabref.GUIGlobals; 
import net.sf.jabref.GeneralRenderer; 
import net.sf.jabref.Globals; 
import net.sf.jabref.JabRefFrame; 
import net.sf.jabref.MetaData; 
import net.sf.jabref.PreviewPanel; 
import net.sf.jabref.Util; 
import net.sf.jabref.external.ExternalFileMenuItem; 
import ca.odell.glazedlists.BasicEventList; 
import ca.odell.glazedlists.EventList; 
import ca.odell.glazedlists.SortedList; 
import ca.odell.glazedlists.event.ListEvent; 
import ca.odell.glazedlists.event.ListEventListener; 
import ca.odell.glazedlists.gui.AdvancedTableFormat; 
import ca.odell.glazedlists.swing.EventSelectionModel; 
import ca.odell.glazedlists.swing.EventTableModel; 
import ca.odell.glazedlists.swing.TableComparatorChooser; 

import com.jgoodies.uif_lite.component.UIFSplitPane; 


public  class  SearchResultsDialog {
	
    private JabRefFrame frame;

	

    private JDialog diag;

	
    private String[] fields = new String[]{
            "author", "title", "year", "journal"
    };

	
    protected final int FILE_COL = 0, URL_COL = 1,
        PAD = 2;

	
    private JLabel fileLabel = new JLabel(GUIGlobals.getImage("psSmall")),
            urlLabel = new JLabel(GUIGlobals.getImage("wwwSmall"));

	

    protected Rectangle toRect = new Rectangle(0, 0, 1, 1);

	

    private EventTableModel<BibtexEntry> model;

	
    private EventList<BibtexEntry> entries = new BasicEventList<BibtexEntry>();

	
    private SortedList<BibtexEntry> sortedEntries;

	
    private HashMap<BibtexEntry, BasePanel> entryHome = new HashMap<BibtexEntry, BasePanel>();

	

    private JTable entryTable;

	
    protected UIFSplitPane contentPane = new UIFSplitPane(UIFSplitPane.VERTICAL_SPLIT);

	
    PreviewPanel preview;

	

    public SearchResultsDialog(JabRefFrame frame, String title) {

        this.frame = frame;

        init(title);
    }


	

    private void init(String title) {
        diag = new JDialog(frame, title, false);

        preview = new PreviewPanel(null, new MetaData(), Globals.prefs.get("preview1"));

        sortedEntries = new SortedList<BibtexEntry>(entries, new EntryComparator(false, true, "author"));
        model = new EventTableModel<BibtexEntry>(sortedEntries,
                new EntryTableFormat());
        entryTable = new JTable(model);
        GeneralRenderer renderer = new GeneralRenderer(Color.white);
        entryTable.setDefaultRenderer(JLabel.class, renderer);
        entryTable.setDefaultRenderer(String.class, renderer);
        setWidths();
        TableComparatorChooser<BibtexEntry> tableSorter =
                new TableComparatorChooser<BibtexEntry>(entryTable, sortedEntries,
                TableComparatorChooser.MULTIPLE_COLUMN_KEYBOARD);
        setupComparatorChooser(tableSorter);
        JScrollPane sp = new JScrollPane(entryTable);

        EventSelectionModel<BibtexEntry> selectionModel = new EventSelectionModel<BibtexEntry>(sortedEntries);
        entryTable.setSelectionModel(selectionModel);
        selectionModel.getSelected().addListEventListener(new EntrySelectionListener());
        entryTable.addMouseListener(new TableClickListener());

        contentPane.setTopComponent(sp);
        contentPane.setBottomComponent(preview);

        
        AbstractAction closeAction = new AbstractAction() {
          public void actionPerformed(ActionEvent e) {
            diag.dispose();
          }
        };
        ActionMap am = contentPane.getActionMap();
        InputMap im = contentPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        im.put(Globals.prefs.getKey("Close dialog"), "close");
        am.put("close", closeAction);
        
        diag.addWindowListener(new WindowAdapter() {
            public void windowOpened(WindowEvent e) {
                contentPane.setDividerLocation(0.5f);
            }

            public void windowClosing(WindowEvent event) {
                Globals.prefs.putInt("searchDialogWidth", diag.getSize().width);
                Globals.prefs.putInt("searchDialogHeight", diag.getSize().height);
            }
        });

        diag.getContentPane().add(contentPane, BorderLayout.CENTER);
        
        diag.setSize(new Dimension(Globals.prefs.getInt("searchDialogWidth"), Globals.prefs
            .getInt("searchDialogHeight")));
        diag.setLocationRelativeTo(frame);
    }


	

    
    public void setVisible(boolean visible) {
        diag.setVisible(visible);
    }


	

    
    public synchronized void clear() {
        entries.clear();
        entryHome.clear();
    }


	

    
    @SuppressWarnings("unchecked")
    protected void setupComparatorChooser(TableComparatorChooser<BibtexEntry> comparatorChooser) {
        
        java.util.List<Comparator<BibtexEntry>> comparators = comparatorChooser
            .getComparatorsForColumn(0);
        comparators.clear();

        comparators = comparatorChooser.getComparatorsForColumn(1);
        comparators.clear();

        
        for (int i = 2; i < PAD; i++) {
            comparators = comparatorChooser.getComparatorsForColumn(i);
            comparators.clear();
            if (i == FILE_COL)
                comparators.add(new IconComparator(new String[] { GUIGlobals.FILE_FIELD }));
            else if (i == URL_COL)
                comparators.add(new IconComparator(new String[] { "url" }));

        }
        
        for (int i = PAD; i < PAD + fields.length; i++) {
            comparators = comparatorChooser.getComparatorsForColumn(i);
            comparators.clear();
            comparators.add(new FieldComparator(fields[i - PAD]));
        }

        sortedEntries.getReadWriteLock().writeLock().lock();
        comparatorChooser.appendComparator(PAD, 0, false);
        sortedEntries.getReadWriteLock().writeLock().unlock();

    }


	

    
    protected void setWidths() {
        TableColumnModel cm = entryTable.getColumnModel();
        for (int i = 0; i < PAD; i++) {
            
            cm.getColumn(i).setPreferredWidth(GUIGlobals.WIDTH_ICON_COL);
            cm.getColumn(i).setMinWidth(GUIGlobals.WIDTH_ICON_COL);
            cm.getColumn(i).setMaxWidth(GUIGlobals.WIDTH_ICON_COL);
        }

        for (int i = 0; i < fields.length; i++) {
            int width = BibtexFields.getFieldLength(fields[i]);
            cm.getColumn(i + PAD).setPreferredWidth(width);
        }
    }


	

    
    public synchronized void addEntries(java.util.List<BibtexEntry> newEntries, BasePanel panel) {
        for (BibtexEntry entry : newEntries) {
            entries.add(entry);
            entryHome.put(entry, panel);
        }
    }


	

    
    public synchronized void addEntry(BibtexEntry entry, BasePanel panel) {
        entries.add(entry);
        entryHome.put(entry, panel);
    }


	

    
     

    
    class  TableClickListener  extends MouseAdapter {
		

        public void mouseReleased(MouseEvent e) {
            if (e.isPopupTrigger()) {
                processPopupTrigger(e);
                return;
            }
        }


		

        public void mousePressed(MouseEvent e) {
            if (e.isPopupTrigger()) {
                processPopupTrigger(e);
                return;
            }

            
            final int row = entryTable.rowAtPoint(e.getPoint());

            
            if (e.getClickCount() == 2) {
                
                BibtexEntry toShow = model.getElementAt(row);
                
                BasePanel p = entryHome.get(toShow);
                
                frame.showBasePanel(p);
                
                p.highlightEntry(toShow);
            }
        }


		

        public void mouseClicked(MouseEvent e) {
            if (e.isPopupTrigger()) {
                processPopupTrigger(e);
                return;
            }
            
            final int col = entryTable.columnAtPoint(e.getPoint()),
                    row = entryTable.rowAtPoint(e.getPoint());
            if (col < PAD) {
                BibtexEntry entry = sortedEntries.get(row);
                BasePanel p = entryHome.get(entry);
                switch (col) {
                    case FILE_COL:
                        Object o = entry.getField(GUIGlobals.FILE_FIELD);
                        if (o != null) {
                            FileListTableModel tableModel = new FileListTableModel();
                            tableModel.setContent((String) o);
                            if (tableModel.getRowCount() == 0)
                                return;
                            FileListEntry fl = tableModel.getEntry(0);
                            (new ExternalFileMenuItem(frame, entry, "", fl.getLink(), null,
                                p.metaData(), fl.getType())).actionPerformed(null);
                        }
                        break;
                    case URL_COL:
                        Object link = entry.getField("url");
                        try {
                            if (link != null)
                                Util.openExternalViewer(p.metaData(), (String) link, "url");
                        } catch (IOException ex) {
                            ex.printStackTrace();
                        }
                        break;

                }
            }
        }


		

        
        public void processPopupTrigger(MouseEvent e) {
            BibtexEntry entry = sortedEntries.get(entryTable.rowAtPoint(e.getPoint()));
            BasePanel p = entryHome.get(entry);
            int col = entryTable.columnAtPoint(e.getPoint());
            JPopupMenu menu = new JPopupMenu();
            int count = 0;

            if (col == FILE_COL) {
                
                Object o = entry.getField(GUIGlobals.FILE_FIELD);
                FileListTableModel fileList = new FileListTableModel();
                fileList.setContent((String)o);
                
                for (int i=0; i<fileList.getRowCount(); i++) {
                    FileListEntry flEntry = fileList.getEntry(i);
                    String description = flEntry.getDescription();
                    if ((description == null) || (description.trim().length() == 0))
                        description = flEntry.getLink();
                    menu.add(new ExternalFileMenuItem(p.frame(), entry, description,
                            flEntry.getLink(), flEntry.getType().getIcon(), p.metaData(),
                            flEntry.getType()));
                    count++;
                }

            }

            if (count > 0)
                menu.show(entryTable, e.getX(), e.getY());
        }



	}

	

    
     

    
    class  EntrySelectionListener implements  ListEventListener<BibtexEntry> {
		

            public void listChanged(ListEvent<BibtexEntry> listEvent) {
                if (listEvent.getSourceList().size() == 1) {
                    BibtexEntry entry = listEvent.getSourceList().get(0);
                    
                    BasePanel p = entryHome.get(entry);
                    
                    preview.setMetaData(p.metaData());
                    
                    preview.setEntry(entry);
                    contentPane.setDividerLocation(0.5f);
                    SwingUtilities.invokeLater(new Runnable() {
                        public void run() {
                            preview.scrollRectToVisible(toRect);
                        }
                    });
                }
            }



	}

	

    
    public  class  EntryTableFormat implements  AdvancedTableFormat<BibtexEntry> {
		

        public int getColumnCount() {
            return PAD+fields.length;
        }


		

        public String getColumnName(int column) {
            if (column >= PAD)
                return Util.nCase(fields[column-PAD]);
            else return "";
        }


		

        public Object getColumnValue(BibtexEntry entry, int column) {
            if (column < PAD) {
                Object o;
                switch (column) {
                    case FILE_COL:
                        o = entry.getField(GUIGlobals.FILE_FIELD);
                        if (o != null) {
                            FileListTableModel model = new FileListTableModel();
                            model.setContent((String) o);
                            fileLabel.setToolTipText(model.getToolTipHTMLRepresentation());
                            if (model.getRowCount() > 0)
                                fileLabel.setIcon(model.getEntry(0).getType().getIcon());
                            return fileLabel;
                        } else
                            return null;
                    case URL_COL:
                        o = entry.getField("url");
                        if (o != null) {
                            urlLabel.setToolTipText((String) o);
                            return urlLabel;
                        } else
                            return null;
                    default:
                        return null;
                }
            }
            else {
                String field = fields[column-PAD];
                if (field.equals("author") || field.equals("editor")) {
                    
                    
                    if (frame.basePanel() != null)
                        return frame.basePanel().tableFormat.formatName
                                (entry.getField(field));
                }
                return entry.getField(field);
            }
        }


		

        public Class<?> getColumnClass(int i) {
            if (i < PAD)
                return JLabel.class;
            else
                return String.class;
        }


		

        public Comparator<?> getColumnComparator(int i) {
            return null;
        }



	}

	

    public void selectFirstEntry() {
        if (entryTable.getRowCount() > 0)
            entryTable.setRowSelectionInterval(0,0);
        else {
            contentPane.setDividerLocation(1.0f);
        }
    }


}
