package net.sf.jabref.gui;

import net.sf.jabref.*;
import net.sf.jabref.external.DownloadExternalFile;
import net.sf.jabref.external.ExternalFileMenuItem;
import net.sf.jabref.groups.GroupTreeNode;
import net.sf.jabref.groups.AllEntriesGroup;
import net.sf.jabref.groups.AbstractGroup;
import net.sf.jabref.groups.UndoableChangeAssignment;
import net.sf.jabref.labelPattern.LabelPatternUtil;
import net.sf.jabref.undo.NamedCompound;
import net.sf.jabref.undo.UndoableInsertEntry;
import net.sf.jabref.undo.UndoableRemoveEntry;

import javax.swing.*;
import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.table.*;
import java.util.*;
import java.util.List;
import java.awt.*;
import java.awt.event.*;
import java.io.IOException;

import com.jgoodies.forms.builder.ButtonBarBuilder;
import com.jgoodies.forms.builder.ButtonStackBuilder;
import com.jgoodies.uif_lite.component.UIFSplitPane;
import ca.odell.glazedlists.*;
import ca.odell.glazedlists.event.ListEventListener;
import ca.odell.glazedlists.event.ListEvent;
import ca.odell.glazedlists.gui.TableFormat;
import ca.odell.glazedlists.swing.TableComparatorChooser;
import ca.odell.glazedlists.swing.EventTableModel;
import ca.odell.glazedlists.swing.EventSelectionModel;



public class ImportInspectionDialog extends JDialog {
	
    private ImportInspectionDialog ths = this;
    private BasePanel panel;
    private JabRefFrame frame;
    private MetaData metaData;
    private UIFSplitPane contentPane = new UIFSplitPane(UIFSplitPane.VERTICAL_SPLIT);
    private JTable glTable;
    private TableComparatorChooser comparatorChooser;
    private EventSelectionModel selectionModel;
    private String[] fields;
    private JProgressBar progressBar = new JProgressBar(JProgressBar.HORIZONTAL);
    private JButton ok = new JButton(Globals.lang("Ok")),
        cancel = new JButton(Globals.lang("Cancel")),
        generate = new JButton(Globals.lang("Generate now"));
    private EventList entries = new BasicEventList();
    private SortedList sortedList;
    private List entriesToDelete = new ArrayList(); 
    private String undoName;
    private ArrayList callBacks = new ArrayList();
    private boolean newDatabase;
    private JMenu groupsAdd = new JMenu(Globals.lang("Add to group"));
    private JPopupMenu popup = new JPopupMenu();
    private JButton selectAll = new JButton(Globals.lang("Select all"));
    private JButton deselectAll = new JButton(Globals.lang("Deselect all"));
    private JButton deselectAllDuplicates = new JButton(Globals.lang("Deselect all duplicates"));
    private JButton stop = new JButton(Globals.lang("Stop"));
    private JButton delete = new JButton(Globals.lang("Delete"));
    private JButton help = new JButton(Globals.lang("Help"));
    private PreviewPanel preview;
    private boolean generatedKeys = false; 
    private boolean defaultSelected = true;
    private Rectangle toRect = new Rectangle(0, 0, 1, 1);
    private Map groupAdditions = new HashMap();
    private JCheckBox autoGenerate = new JCheckBox(Globals.lang("Generate keys"), Globals.prefs.getBoolean("generateKeysAfterInspection"));
    private JLabel
        duplLabel = new JLabel(GUIGlobals.getImage("duplicate")),
        fileLabel = new JLabel(GUIGlobals.getImage("psSmall")),
        pdfLabel = new JLabel(GUIGlobals.getImage("pdfSmall")),
        psLabel = new JLabel(GUIGlobals.getImage("psSmall")),
        urlLabel = new JLabel(GUIGlobals.getImage("wwwSmall"));

    private final int
        DUPL_COL = 1,
        FILE_COL = 2,
        PDF_COL = -1,
        PS_COL = -2,
        URL_COL = 3,
        PAD = 4; 

    
    public void setDefaultSelected(boolean defaultSelected) {
        this.defaultSelected = defaultSelected;
    }

    
    public ImportInspectionDialog(JabRefFrame frame, BasePanel panel, String[] fields,
                                  String undoName, boolean newDatabase) {
        this.frame = frame;
        this.panel = panel;
        this.metaData = (panel != null) ? panel.metaData() : new MetaData();
        this.fields = fields;
        this.undoName = undoName;
        this.newDatabase = newDatabase;
        preview = new PreviewPanel(metaData, Globals.prefs.get("preview1"));

        duplLabel.setToolTipText(Globals.lang("Possible duplicate of existing entry. Click to resolve."));

        sortedList = new SortedList(entries);
        EventTableModel tableModelGl = new EventTableModel(sortedList,
                new EntryTableFormat());
        glTable = new EntryTable(tableModelGl);
        GeneralRenderer renderer = new GeneralRenderer(Color.white);
        glTable.setDefaultRenderer(JLabel.class, renderer);
        glTable.setDefaultRenderer(String.class, renderer);
        glTable.getInputMap().put(Globals.prefs.getKey("Delete"), "delete");
        DeleteListener deleteListener = new DeleteListener();
        glTable.getActionMap().put("delete", deleteListener);

        selectionModel = new EventSelectionModel(sortedList);
        glTable.setSelectionModel(selectionModel);
        selectionModel.getSelected().addListEventListener(new EntrySelectionListener());
        comparatorChooser = new TableComparatorChooser(glTable, sortedList,
                TableComparatorChooser.MULTIPLE_COLUMN_KEYBOARD);
        setupComparatorChooser();
        glTable.addMouseListener(new TableClickListener());

        setWidths();

        getContentPane().setLayout(new BorderLayout());
        progressBar.setIndeterminate(true);
        JPanel centerPan = new JPanel();
        centerPan.setLayout(new BorderLayout());

        contentPane.setTopComponent(new JScrollPane(glTable));
        contentPane.setBottomComponent(new JScrollPane(preview));

        centerPan.add(contentPane, BorderLayout.CENTER);
        centerPan.add(progressBar, BorderLayout.SOUTH);

        popup.add(deleteListener);
        popup.addSeparator();
        if (!newDatabase) {
            GroupTreeNode node = metaData.getGroups();
            groupsAdd.setEnabled(false); 
            insertNodes(groupsAdd, node, true);
            popup.add(groupsAdd);
        }

        
        popup.add(new LinkLocalFile());
        popup.add(new DownloadFile());
        popup.add(new AutoSetLinks());
        
        
        popup.add(new AttachUrl());
        getContentPane().add(centerPan, BorderLayout.CENTER);


        ButtonBarBuilder bb = new ButtonBarBuilder();
        bb.addGlue();
        bb.addGridded(ok);
        bb.addGridded(stop);
        bb.addGridded(cancel);
        bb.addRelatedGap();
        bb.addGridded(help);
        bb.addGlue();
        bb.getPanel().setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));

        ButtonStackBuilder builder = new ButtonStackBuilder();
        builder.addGridded(selectAll);
        builder.addGridded(deselectAll);
        builder.addGridded(deselectAllDuplicates);
        builder.addRelatedGap();
        builder.addGridded(delete);
        builder.addRelatedGap();
        builder.addGridded(autoGenerate);
        builder.addGridded(generate);
        builder.getPanel().setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
        centerPan.add(builder.getPanel(), BorderLayout.WEST);

        ok.setEnabled(false);
        generate.setEnabled(false);
        ok.addActionListener(new OkListener());
        cancel.addActionListener(new CancelListener());
        generate.addActionListener(new GenerateListener());
        stop.addActionListener(new StopListener());
        selectAll.addActionListener(new SelectionButton(true));
        deselectAll.addActionListener(new SelectionButton(false));
        deselectAllDuplicates.addActionListener(new DeselectDuplicatesButtonListener());
        deselectAllDuplicates.setEnabled(false);
        delete.addActionListener(deleteListener);
        help.addActionListener(new HelpAction(frame.helpDiag, GUIGlobals.importInspectionHelp));
        getContentPane().add(bb.getPanel(), BorderLayout.SOUTH);

        
        setSize(new Dimension(Globals.prefs.getInt("importInspectionDialogWidth"),
                Globals.prefs.getInt("importInspectionDialogHeight")));
        addWindowListener(new WindowAdapter() {

            public void windowOpened(WindowEvent e) {
                contentPane.setDividerLocation(0.5f);
            }

            public void windowClosed(WindowEvent e) {
                Globals.prefs.putInt("importInspectionDialogWidth", getSize().width);
                Globals.prefs.putInt("importInspectionDialogHeight", getSize().height);
            }
        });

    }

    public void setProgress(int current, int max) {
        progressBar.setIndeterminate(false);
        progressBar.setMinimum(0);
        progressBar.setMaximum(max);
        progressBar.setValue(current);
    }

    
    public void addEntry(BibtexEntry entry) {
        List list = new ArrayList();
        list.add(entry);
        addEntries(list);
    }

    
    public void addEntries(Collection entries) {

        for (Iterator i = entries.iterator(); i.hasNext();) {
            BibtexEntry entry = (BibtexEntry) i.next();
            
            entry.setSearchHit(defaultSelected);
            
            
            
            if (((panel != null) && (Util.containsDuplicate(panel.database(), entry) != null))
                || (internalDuplicate(this.entries, entry) != null)) {
                entry.setGroupHit(true);
                deselectAllDuplicates.setEnabled(true);
            }
            this.entries.getReadWriteLock().writeLock().lock();
            this.entries.add(entry);
            this.entries.getReadWriteLock().writeLock().unlock();
        }
    }

    
    protected BibtexEntry internalDuplicate(Collection entries, BibtexEntry entry) {
        for (Iterator iterator = entries.iterator(); iterator.hasNext();) {
            BibtexEntry othEntry = (BibtexEntry) iterator.next();
            if (othEntry == entry)
                continue; 
            if (Util.isDuplicate(entry, othEntry, Globals.duplicateThreshold))
                return othEntry;
        }
        return null;
    }

    
    public void removeSelectedEntries() {
        int row = glTable.getSelectedRow();
        List toRemove = new ArrayList();
        toRemove.addAll(selectionModel.getSelected());
        entries.getReadWriteLock().writeLock().lock();
        for (Iterator i=toRemove.iterator(); i.hasNext();) {
            entries.remove(i.next());
        }
        entries.getReadWriteLock().writeLock().unlock();
        glTable.clearSelection();
        if ((row >= 0) && (entries.size() > 0)) {
            row = Math.min(entries.size()-1, row);
            glTable.addRowSelectionInterval(row, row);
        }
    }

    
    public void entryListComplete() {
        progressBar.setIndeterminate(false);
        progressBar.setVisible(false);
        ok.setEnabled(true);
        if (!generatedKeys)
            generate.setEnabled(true);
        stop.setEnabled(false);
    }


    
    public List getSelectedEntries() {
        List selected = new ArrayList();
        for (Iterator i=entries.iterator(); i.hasNext();) {
            BibtexEntry entry = (BibtexEntry)i.next();
            if (entry.isSearchHit())
                selected.add(entry);
        }
        
        return selected;
    }

    
    public void generateKeySelectedEntry() {
        if (selectionModel.getSelected().size() != 1)
                return;
        BibtexEntry entry = (BibtexEntry) selectionModel.getSelected().get(0);
        entries.getReadWriteLock().writeLock().lock();
        BibtexDatabase database = null;
        
        if (panel != null)
            database = panel.database();
        
        else
            database = new BibtexDatabase();
        try {
            entry.setId(Util.createNeutralId());
            
            database.insertEntry(entry);
        } catch (KeyCollisionException ex) {
            ex.printStackTrace();
        }
        
        LabelPatternUtil.makeLabel(Globals.prefs.getKeyPattern(), database, entry);
        
        
        database.removeEntry(entry.getId());
        
        entries.getReadWriteLock().writeLock().lock();
        glTable.repaint();
    }

    
    public void generateKeys(boolean addColumn) {
        entries.getReadWriteLock().writeLock().lock();
        BibtexDatabase database = null;
        
        if (panel != null)
            database = panel.database();
        
        else
            database = new BibtexDatabase();
        List keys = new ArrayList(entries.size());
        
        
        for (Iterator i = entries.iterator(); i.hasNext();) {
            BibtexEntry entry = (BibtexEntry) i.next();
            
            try {
                entry.setId(Util.createNeutralId());
                database.insertEntry(entry);
            } catch (KeyCollisionException ex) {
                ex.printStackTrace();
            }
            
            LabelPatternUtil.makeLabel(Globals.prefs.getKeyPattern(), database, entry);
            
            keys.add(entry.getCiteKey());
        }
        
        
        
        for (Iterator i = entries.iterator(); i.hasNext();) {
            BibtexEntry entry = (BibtexEntry) i.next();
            database.removeEntry(entry.getId());
        }
        entries.getReadWriteLock().writeLock().lock();
        glTable.repaint();
    }


    public void insertNodes(JMenu menu, GroupTreeNode node, boolean add) {
        final AbstractAction action = getAction(node, add);

        if (node.getChildCount() == 0) {
            menu.add(action);
            if (action.isEnabled())
                menu.setEnabled(true);
            return;
        }

        JMenu submenu = null;
        if (node.getGroup() instanceof AllEntriesGroup) {
            for (int i = 0; i < node.getChildCount(); ++i) {
                insertNodes(menu, (GroupTreeNode) node.getChildAt(i), add);
            }
        } else {
            submenu = new JMenu("[" + node.getGroup().getName() + "]");
            
            
            submenu.setEnabled(action.isEnabled());
            submenu.add(action);
            submenu.add(new JPopupMenu.Separator());
            for (int i = 0; i < node.getChildCount(); ++i)
                insertNodes(submenu, (GroupTreeNode) node.getChildAt(i), add);
            menu.add(submenu);
            if (submenu.isEnabled())
                menu.setEnabled(true);
        }
    }

    private AbstractAction getAction(GroupTreeNode node, boolean add) {
        AbstractAction action = add ? (AbstractAction) new AddToGroupAction(node)
                : (AbstractAction) new RemoveFromGroupAction(node);
        AbstractGroup group = node.getGroup();
        action.setEnabled(group.supportsAdd());
        
        return action;
    }

    
    class AddToGroupAction extends AbstractAction {
        private GroupTreeNode node;

        public AddToGroupAction(GroupTreeNode node) {
            super(node.getGroup().getName());
            this.node = node;
        }

        public void actionPerformed(ActionEvent event) {

            selectionModel.getSelected().getReadWriteLock().writeLock().lock();
            for (Iterator i=selectionModel.getSelected().iterator(); i.hasNext();) {
                BibtexEntry entry = (BibtexEntry)i.next();
                
                Set groups = (Set) groupAdditions.get(entry);
                if (groups == null) {
                    
                    groups = new HashSet();
                    groupAdditions.put(entry, groups);
                }
                
                groups.add(node);
            }
            selectionModel.getSelected().getReadWriteLock().writeLock().unlock();
        }
    }

    class RemoveFromGroupAction extends AbstractAction {
        private GroupTreeNode node;

        public RemoveFromGroupAction(GroupTreeNode node) {
            this.node = node;
        }

        public void actionPerformed(ActionEvent event) {
        }
    }

    public void addCallBack(CallBack cb) {
        callBacks.add(cb);
    }

    class OkListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {

            
            
            if (Globals.prefs.getBoolean("warnAboutDuplicatesInInspection")) {
                for (Iterator i=entries.iterator(); i.hasNext();) {

                    BibtexEntry entry = (BibtexEntry)i.next();
                    
                    
                    if (!entry.isSearchHit())
                        continue;

                    
                    
                    if (entry.isGroupHit()) {
                        CheckBoxMessage cbm = new CheckBoxMessage(
                                Globals.lang("There are possible duplicates (marked with a 'D' icon) that haven't been resolved. Continue?"),
                                Globals.lang("Disable this confirmation dialog"), false);
                        int answer = JOptionPane.showConfirmDialog(ImportInspectionDialog.this, cbm, Globals.lang("Duplicates found"),
                                    JOptionPane.YES_NO_OPTION);
                        if (cbm.isSelected())
                            Globals.prefs.putBoolean("warnAboutDuplicatesInInspection", false);
                        if (answer == JOptionPane.NO_OPTION)
                            return;
                        break;
                    }
                }
            }

            
            NamedCompound ce = new NamedCompound(undoName);

            
            if (entriesToDelete.size() > 0) {
                for (Iterator i=entriesToDelete.iterator(); i.hasNext();) {
                    BibtexEntry entry = (BibtexEntry)i.next();
                    ce.addEdit(new UndoableRemoveEntry(panel.database(), entry, panel));
                    panel.database().removeEntry(entry.getId());
                }
            }
            


            
            if (autoGenerate.isSelected() && !generatedKeys) {
                generateKeys(false);
            }
            
            Globals.prefs.putBoolean("generateKeysAfterInspection", autoGenerate.isSelected());

            final List selected = getSelectedEntries();

            if (selected.size() > 0) {

                if (newDatabase) {
                    
                    BibtexDatabase base = new BibtexDatabase();
                    panel = new BasePanel(frame, base, null, new HashMap(), Globals.prefs.get("defaultEncoding"));
                }

                boolean groupingCanceled = false;

                
                Util.setAutomaticFields(selected, Globals.prefs.getBoolean("overwriteOwner"),
                    Globals.prefs.getBoolean("overwriteTimeStamp"));


                for (Iterator i = selected.iterator(); i.hasNext();) {
                    BibtexEntry entry = (BibtexEntry) i.next();
                    

                    
                    entry.setSearchHit(false);
                    entry.setGroupHit(false);

                    
                    Set groups = (Set) groupAdditions.get(entry);
                    if (!groupingCanceled && (groups != null)) {
                        if (entry.getField(BibtexFields.KEY_FIELD) == null) {
                            
                            
                            
                           int answer = JOptionPane.showConfirmDialog(ImportInspectionDialog.this,
                                   Globals.lang("Cannot add entries to group without generating keys. Generate keys now?"),
                                    Globals.lang("Add to group"), JOptionPane.YES_NO_OPTION);
                            if (answer == JOptionPane.YES_OPTION) {
                                generateKeys(false);
                            } else
                                groupingCanceled = true;
                        }

                        
                        if (entry.getField(BibtexFields.KEY_FIELD) != null) {
                            for (Iterator i2 = groups.iterator(); i2.hasNext();) {
                                GroupTreeNode node = (GroupTreeNode) i2.next();
                                if (node.getGroup().supportsAdd()) {
                                    
                                    AbstractUndoableEdit undo = node.getGroup().add(new BibtexEntry[]{entry});
                                    if (undo instanceof UndoableChangeAssignment)
                                        ((UndoableChangeAssignment) undo).setEditedNode(node);
                                    ce.addEdit(undo);

                                } else {
                                    
                                }
                            }
                        }
                    }

                    try {
                        entry.setId(Util.createNeutralId());
                        panel.database().insertEntry(entry);
                        
                        Util.updateCompletersForEntry(panel.getAutoCompleters(), entry);
                        ce.addEdit(new UndoableInsertEntry(panel.database(), entry, panel));
                    } catch (KeyCollisionException e) {
                        e.printStackTrace();
                    }
                }

                ce.end();
                panel.undoManager.addEdit(ce);
            }

            dispose();
            SwingUtilities.invokeLater(new Thread() {
                public void run() {
                    if (newDatabase) {
                        frame.addTab(panel, null, true);
                    }
                    panel.markBaseChanged();
                    for (Iterator i = callBacks.iterator(); i.hasNext();) {
                        ((CallBack) i.next()).done(selected.size());
                    }
                }
            });

        }

    }

    private void signalStopFetching() {
        for (Iterator i = callBacks.iterator(); i.hasNext();) {
            ((CallBack) i.next()).stopFetching();
        }
    }

    private void setWidths() {
        TableColumnModel cm = glTable.getColumnModel();
        cm.getColumn(0).setPreferredWidth(55);
        cm.getColumn(0).setMinWidth(55);
        cm.getColumn(0).setMaxWidth(55);
        for (int i = 1; i < PAD; i++) {
            
            cm.getColumn(i).setPreferredWidth(GUIGlobals.WIDTH_ICON_COL);
            cm.getColumn(i).setMinWidth(GUIGlobals.WIDTH_ICON_COL);
            cm.getColumn(i).setMaxWidth(GUIGlobals.WIDTH_ICON_COL);
        }

        for (int i = 0; i < fields.length; i++) {
            int width = BibtexFields.getFieldLength( fields[i]) ;
            glTable.getColumnModel().getColumn(i + PAD).setPreferredWidth(width);
        }
    }



    class StopListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            signalStopFetching();
            entryListComplete();
        }
    }

    class CancelListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            signalStopFetching();
            dispose();
            for (Iterator i = callBacks.iterator(); i.hasNext();) {
                ((CallBack) i.next()).cancelled();
            }
        }
    }

    class GenerateListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            generate.setEnabled(false);
            generatedKeys = true; 
            generateKeys(true); 
        }
    }

    class DeleteListener extends AbstractAction implements ActionListener {
        public DeleteListener() {
            super(Globals.lang("Delete"), GUIGlobals.getImage("delete"));
        }

        public void actionPerformed(ActionEvent event) {
            removeSelectedEntries();
        }
    }

    class MyTable extends JTable {
        public MyTable(TableModel model) {
            super(model);
            
        }

        public boolean isCellEditable(int row, int col) {
            return col == 0;
        }
    }

    class MyTableModel extends DefaultTableModel {


        public Class getColumnClass(int i) {
            if (i == 0)
                return Boolean.class;
            else
                return String.class;
        }

    }

    class SelectionButton implements ActionListener {
        private Boolean enable;

        public SelectionButton(boolean enable) {
            this.enable = Boolean.valueOf(enable);
        }

        public void actionPerformed(ActionEvent event) {
            for (int i = 0; i < glTable.getRowCount(); i++) {
                glTable.setValueAt(enable, i, 0);
            }
            glTable.repaint();
        }
    }
    
    class DeselectDuplicatesButtonListener implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            for (int i = 0; i < glTable.getRowCount(); i++) {
                if(glTable.getValueAt(i, DUPL_COL) != null){
                	glTable.setValueAt(Boolean.valueOf(false), i, 0);
                }
            }
            glTable.repaint();
        }
    }
    
    class EntrySelectionListener implements ListEventListener {

        public void listChanged(ListEvent listEvent) {
            if (listEvent.getSourceList().size() == 1) {
                preview.setEntry((BibtexEntry) listEvent.getSourceList().get(0));
                contentPane.setDividerLocation(0.5f);
                SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        preview.scrollRectToVisible(toRect);
                    }
                });
            }
        }
    }


    
    class TableClickListener implements MouseListener {

        public boolean isIconColumn(int col) {
            return (col == FILE_COL) || (col == PDF_COL) || (col == PS_COL)
                    || (col == URL_COL);
        }

        public void mouseClicked(MouseEvent e) {
            final int col = glTable.columnAtPoint(e.getPoint()),
              row = glTable.rowAtPoint(e.getPoint());
            if (isIconColumn(col)) {
                BibtexEntry entry = (BibtexEntry)sortedList.get(row);

                switch (col) {
                    case FILE_COL:
                        Object o = entry.getField(GUIGlobals.FILE_FIELD);
                        if (o != null) {
                            FileListTableModel tableModel = new FileListTableModel();
                            tableModel.setContent((String)o);
                            if (tableModel.getRowCount() == 0)
                                return;
                            FileListEntry fl = tableModel.getEntry(0);
                            (new ExternalFileMenuItem(frame, entry, "", fl.getLink(), null, panel.metaData(), fl.getType())).
                                    actionPerformed(null);
                        }
                        break;
                    case URL_COL:
                        openExternalLink("url", e);
                        break;
                    case PDF_COL:
                        openExternalLink("pdf", e);
                        break;
                    case PS_COL:
                        openExternalLink("ps", e);
                        break;
                }
            }
        }

        public void mouseEntered(MouseEvent e) {

        }

        public void mouseExited(MouseEvent e) {

        }

        
        public void showPopup(MouseEvent e) {
            final int col = glTable.columnAtPoint(e.getPoint());
            switch (col) {
                case FILE_COL:
                    showFileFieldMenu(e);
                    break;
                default:
                    showOrdinaryRightClickMenu(e);
                    break;
            }

        }

        public void showOrdinaryRightClickMenu(MouseEvent e) {
            popup.show(glTable, e.getX(), e.getY());
        }

        
        public void showFileFieldMenu(MouseEvent e) {
            final int row = glTable.rowAtPoint(e.getPoint());
            BibtexEntry entry = (BibtexEntry)sortedList.get(row);
            JPopupMenu menu = new JPopupMenu();
            int count = 0;
            Object o = entry.getField(GUIGlobals.FILE_FIELD);
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
            if (count == 0) {
                showOrdinaryRightClickMenu(e);
            }
            else
                menu.show(glTable, e.getX(), e.getY());
        }

        
        public void openExternalLink(String fieldName, MouseEvent e) {
            final int row = glTable.rowAtPoint(e.getPoint());
            BibtexEntry entry = (BibtexEntry)sortedList.get(row);

            Object link = entry.getField(fieldName);
            try {
                if (link != null)
                    Util.openExternalViewer(panel.metaData(), (String) link, fieldName);
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }


        public void mouseReleased(MouseEvent e) {
            
            if (e.isPopupTrigger()) {
                showPopup(e);
                return;
            }
        }

        public void mousePressed(MouseEvent e) {
            
            if (e.isPopupTrigger()) {
                showPopup(e);
                return;
            }

            
            final int col = glTable.columnAtPoint(e.getPoint()),
              row = glTable.rowAtPoint(e.getPoint());
            
            if ((col == DUPL_COL) && (glTable.getValueAt(row, col) != null)) {
                BibtexEntry first = (BibtexEntry)sortedList.get(row);
                BibtexEntry other = Util.containsDuplicate(panel.database(), first);
                if (other != null) {
                    
                    
                    DuplicateResolverDialog diag = new DuplicateResolverDialog
                            (ImportInspectionDialog.this, other, first, DuplicateResolverDialog.INSPECTION);
                    Util.placeDialog(diag, ImportInspectionDialog.this);
                    diag.setVisible(true);
                    ImportInspectionDialog.this.toFront();
                    if (diag.getSelected() == DuplicateResolverDialog.KEEP_UPPER) {
                        
                        
                        entriesToDelete.add(other);
                        
                        
                        entries.getReadWriteLock().writeLock().lock();
                        first.setGroupHit(false);
                        entries.getReadWriteLock().writeLock().unlock();

                    } else if (diag.getSelected() == DuplicateResolverDialog.KEEP_LOWER) {
                        
                        entries.getReadWriteLock().writeLock().lock();
                        entries.remove(first);
                        entries.getReadWriteLock().writeLock().unlock();
                    } else if (diag.getSelected() == DuplicateResolverDialog.KEEP_BOTH) {
                        
                        entries.getReadWriteLock().writeLock().lock();
                        first.setGroupHit(false);
                        entries.getReadWriteLock().writeLock().unlock();
                    }
                }
                
                other = internalDuplicate(entries, first);
                if (other != null) {
                    int answer = DuplicateResolverDialog.resolveDuplicate
                            (ImportInspectionDialog.this, first, other);
                    if (answer == DuplicateResolverDialog.KEEP_UPPER) {
                        entries.remove(other);
                        first.setGroupHit(false);
                    } else if (answer == DuplicateResolverDialog.KEEP_LOWER) {
                        entries.remove(first);
                    } else if (answer == DuplicateResolverDialog.KEEP_BOTH) {
                        first.setGroupHit(false);
                    }
                }
            }
        }
    }

    class AttachUrl extends JMenuItem implements ActionListener {
        public AttachUrl() {
            super(Globals.lang("Attach URL"));
            addActionListener(this);
        }

        public void actionPerformed(ActionEvent event) {
            if (selectionModel.getSelected().size() != 1)
                return;
            BibtexEntry entry = (BibtexEntry) selectionModel.getSelected().get(0);
            String result = JOptionPane.showInputDialog(ths, Globals.lang("Enter URL"), entry.getField("url"));
            entries.getReadWriteLock().writeLock().lock();
            if (result != null) {
                if (result.equals("")) {
                    entry.clearField("url");
                } else {
                    entry.setField("url", result);
                }
            }
            entries.getReadWriteLock().writeLock().unlock();
            glTable.repaint();
        }
    }

    class DownloadFile extends JMenuItem implements ActionListener,
        DownloadExternalFile.DownloadCallback {

        BibtexEntry entry = null;

        public DownloadFile() {
            super(Globals.lang("Download file"));
            addActionListener(this);
        }

        public void actionPerformed(ActionEvent actionEvent) {
            if (selectionModel.getSelected().size() != 1)
                return;
            entry = (BibtexEntry) selectionModel.getSelected().get(0);
            String bibtexKey = entry.getCiteKey();
            if (bibtexKey == null) {
                int answer = JOptionPane.showConfirmDialog(frame,
                        Globals.lang("This entry has no BibTeX key. Generate key now?"),
                        Globals.lang("Download file"), JOptionPane.OK_CANCEL_OPTION,
                        JOptionPane.QUESTION_MESSAGE);
                if (answer == JOptionPane.OK_OPTION) {
                    generateKeySelectedEntry();
                    bibtexKey = entry.getCiteKey();
                }
            }
            DownloadExternalFile def = new DownloadExternalFile(frame, metaData, bibtexKey);
            try {
                def.download(this);
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }

        public void downloadComplete(FileListEntry file) {
            ImportInspectionDialog.this.toFront(); 
            FileListTableModel model = new FileListTableModel();
            String oldVal = (String)entry.getField(GUIGlobals.FILE_FIELD);
            if (oldVal != null)
                model.setContent(oldVal);
            model.addEntry(model.getRowCount(), file);
            entries.getReadWriteLock().writeLock().lock();
            entry.setField(GUIGlobals.FILE_FIELD, model.getStringRepresentation());
            entries.getReadWriteLock().writeLock().unlock();
            glTable.repaint();
        }
    }

    class AutoSetLinks extends JMenuItem implements ActionListener {

        public AutoSetLinks() {
            super(Globals.lang("Autoset external links"));
            addActionListener(this);
        }

        public void actionPerformed(ActionEvent actionEvent) {
            if (selectionModel.getSelected().size() != 1)
                return;
            final BibtexEntry entry = (BibtexEntry) selectionModel.getSelected().get(0);
            String bibtexKey = entry.getCiteKey();
            if (bibtexKey == null) {
                int answer = JOptionPane.showConfirmDialog(frame,
                        Globals.lang("This entry has no BibTeX key. Generate key now?"),
                        Globals.lang("Download file"), JOptionPane.OK_CANCEL_OPTION,
                        JOptionPane.QUESTION_MESSAGE);
                if (answer == JOptionPane.OK_OPTION) {
                    generateKeySelectedEntry();
                    bibtexKey = entry.getCiteKey();
                } else return; 
            }
            final FileListTableModel model = new FileListTableModel();
            String oldVal = (String)entry.getField(GUIGlobals.FILE_FIELD);
            if (oldVal != null)
                model.setContent(oldVal);
            
            JDialog diag = new JDialog(ImportInspectionDialog.this, true);
            FileListEditor.autoSetLinks(entry, model, metaData, new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    if (e.getID() > 0) {
                        entries.getReadWriteLock().writeLock().lock();
                        entry.setField(GUIGlobals.FILE_FIELD, model.getStringRepresentation());
                        entries.getReadWriteLock().writeLock().unlock();
                        glTable.repaint();
                    }
                }
            }, diag);

        }
    }

    class LinkLocalFile extends JMenuItem implements ActionListener,
        DownloadExternalFile.DownloadCallback {

        BibtexEntry entry = null;

        public LinkLocalFile() {
            super(Globals.lang("Link local file"));
            addActionListener(this);
        }

        public void actionPerformed(ActionEvent actionEvent) {
            if (selectionModel.getSelected().size() != 1)
                return;
            entry = (BibtexEntry) selectionModel.getSelected().get(0);
            FileListEntry flEntry = new FileListEntry("", "", null);
            FileListEntryEditor editor = new FileListEntryEditor(frame, flEntry, false, metaData);
            editor.setVisible(true);                                                 
            if (editor.okPressed()) {
                FileListTableModel model = new FileListTableModel();
                String oldVal = (String)entry.getField(GUIGlobals.FILE_FIELD);
                if (oldVal != null)
                    model.setContent(oldVal);
                model.addEntry(model.getRowCount(), flEntry);
                entries.getReadWriteLock().writeLock().lock();
                entry.setField(GUIGlobals.FILE_FIELD, model.getStringRepresentation());
                entries.getReadWriteLock().writeLock().unlock();
                glTable.repaint();
            }
        }

        public void downloadComplete(FileListEntry file) {
            ImportInspectionDialog.this.toFront(); 
            FileListTableModel model = new FileListTableModel();
            String oldVal = (String)entry.getField(GUIGlobals.FILE_FIELD);
            if (oldVal != null)
                model.setContent(oldVal);
            model.addEntry(model.getRowCount(), file);
            entries.getReadWriteLock().writeLock().lock();
            entry.setField(GUIGlobals.FILE_FIELD, model.getStringRepresentation());
            entries.getReadWriteLock().writeLock().unlock();
            glTable.repaint();
        }
    }

    class AttachFile extends JMenuItem implements ActionListener {
        String fileType;

        public AttachFile(String fileType) {
            super(Globals.lang("Attach %0 file", new String[]{fileType.toUpperCase()}));
            this.fileType = fileType;
            addActionListener(this);
        }

        public void actionPerformed(ActionEvent event) {

            if (selectionModel.getSelected().size() != 1)
                return;
            BibtexEntry entry = (BibtexEntry) selectionModel.getSelected().get(0);
            
            AttachFileDialog diag = new AttachFileDialog(ths, metaData, entry, fileType);
            Util.placeDialog(diag, ths);
            diag.setVisible(true);
            
            if (!diag.cancelled()) {
                entries.getReadWriteLock().writeLock().lock();
                entry.setField(fileType, diag.getValue());
                entries.getReadWriteLock().writeLock().unlock();
                glTable.repaint();
            }

        }
    }

    public static interface CallBack {
        
        
        
        public void done(int entriesImported);

        
        public void cancelled();

        
        
        
        public void stopFetching();
    }


    private void setupComparatorChooser() {
        
        java.util.List comparators = comparatorChooser.getComparatorsForColumn(0);
        comparators.clear();

        comparators = comparatorChooser.getComparatorsForColumn(1);
        comparators.clear();

        
        for (int i = 2; i < PAD; i++) {
            comparators = comparatorChooser.getComparatorsForColumn(i);
            comparators.clear();
            if (i == FILE_COL)
                comparators.add(new IconComparator(new String[] {GUIGlobals.FILE_FIELD}));
            else if (i == PDF_COL)
                comparators.add(new IconComparator(new String[] {"pdf"}));
            else if (i == PS_COL)
                comparators.add(new IconComparator(new String[] {"ps"}));
            else if (i == URL_COL)
                comparators.add(new IconComparator(new String[] {"url"}));

        }
        
        for (int i = PAD; i < PAD+fields.length; i++) {
            comparators = comparatorChooser.getComparatorsForColumn(i);
            comparators.clear();
            comparators.add(new FieldComparator(fields[i-PAD]));
        }

        

        
        sortedList.getReadWriteLock().writeLock().lock();
        comparatorChooser.appendComparator(PAD, 0, false);
        sortedList.getReadWriteLock().writeLock().unlock();

    }

    class EntryTable extends JTable {
        GeneralRenderer renderer = new GeneralRenderer(Color.white);
        public EntryTable(TableModel model) {
            super(model);
        }
        public TableCellRenderer getCellRenderer(int row, int column) {
            return column == 0 ? getDefaultRenderer(Boolean.class) : renderer;
        }

        

        public Class getColumnClass(int col) {
            if (col == 0)
                return Boolean.class;
            else if (col < PAD)
                return JLabel.class;
            else return String.class;
        }

        public boolean isCellEditable(int row, int column) {
            return column == 0;
        }

        public void setValueAt(Object value, int row, int column) {
            
            entries.getReadWriteLock().writeLock().lock();
            BibtexEntry entry = (BibtexEntry)sortedList.get(row);
            entry.setSearchHit(((Boolean)value).booleanValue());
            entries.getReadWriteLock().writeLock().unlock();
        }
    }

    class EntryTableFormat implements TableFormat {
        public int getColumnCount() {
            return PAD+fields.length;
        }

        public String getColumnName(int i) {
            if (i == 0)
                return Globals.lang("Keep");
            if (i >= PAD) {
                return Util.nCase(fields[i-PAD]);
            }
            return "";
        }

        public Object getColumnValue(Object object, int i) {
            BibtexEntry entry = (BibtexEntry)object;
            if (i == 0)
                return entry.isSearchHit() ? Boolean.TRUE : Boolean.FALSE;
            else if (i < PAD) {
                Object o;
                switch (i) {
                    case DUPL_COL: return entry.isGroupHit() ?  duplLabel : null;
                    case FILE_COL:
                        o = entry.getField(GUIGlobals.FILE_FIELD);
                        if (o != null) {
                            FileListTableModel model = new FileListTableModel();
                            model.setContent((String)o);
                            fileLabel.setToolTipText(model.getToolTipHTMLRepresentation());
                            return fileLabel;
                        } else return null;
                    case PDF_COL:
                        o = entry.getField("pdf");
                        if (o != null) {
                            pdfLabel.setToolTipText((String)o);
                            return pdfLabel;
                        } else return null;

                    case PS_COL:
                        o = entry.getField("ps");
                        if (o != null) {
                            psLabel.setToolTipText((String)o);
                            return psLabel;
                        } else return null;
                    case URL_COL:
                        o = entry.getField("url");
                        if (o != null) {
                            urlLabel.setToolTipText((String)o);
                            return urlLabel;
                        } else return null;
                    default: return null;
                }
            }
            else {
                String field = fields[i-PAD];
                if (field.equals("author") || field.equals("editor")) {
                    String contents = (String)entry.getField(field);
                    return (contents != null) ?
                        AuthorList.fixAuthor_Natbib(contents) : "";
                }
                else
                    return entry.getField(field);
            }
        }

    }
}
