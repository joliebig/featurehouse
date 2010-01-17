
package net.sf.jabref; 

import java.awt.*; 
import java.awt.datatransfer.StringSelection; 
import java.awt.dnd.*; 
import java.awt.event.*; 
import java.beans.*; 
import java.io.*; 
import java.util.*; 
import java.util.List; 
import java.util.logging.Logger; 

import javax.swing.*; 
import javax.swing.event.*; 
import javax.swing.text.JTextComponent; 

import net.sf.jabref.export.LatexFieldFormatter; 
import net.sf.jabref.groups.*; 
import net.sf.jabref.imports.BibtexParser; 
import net.sf.jabref.labelPattern.LabelPatternUtil; 
import net.sf.jabref.undo.*; 
import net.sf.jabref.external.ExternalFilePanel; 
import net.sf.jabref.journals.JournalAbbreviations; 
import net.sf.jabref.gui.date.*; 
import net.sf.jabref.gui.AutoCompleter; 
import com.jgoodies.forms.builder.DefaultFormBuilder; 
import com.jgoodies.forms.layout.FormLayout; 
import java.awt.dnd.DnDConstants; 
import java.awt.dnd.DropTarget; 
import java.beans.PropertyChangeEvent; 
import java.beans.VetoableChangeListener; 
import java.io.File; 
import java.io.IOException; 
import java.io.StringWriter; 
import java.util.ArrayList; 
import java.util.HashSet; 
import java.util.Iterator; 
import javax.swing.event.ChangeEvent; 
import javax.swing.event.ChangeListener; 
import net.sf.jabref.gui.date.DatePickerButton; 
import net.sf.jabref.undo.NamedCompound; 
import net.sf.jabref.undo.UndoableFieldChange; 
import net.sf.jabref.undo.UndoableKeyChange; 
import net.sf.jabref.undo.UndoableRemoveEntry; 


public  class  EntryEditor  extends JPanel implements  VetoableChangeListener {
	

    
    private BibtexEntry entry;

	

    BibtexEntryType type;

	

    
    CloseAction closeAction;

	

    
    DeleteAction deleteAction = new DeleteAction();

	

    
    CopyKeyAction copyKeyAction;

	

    
    AbstractAction nextEntryAction = new NextEntryAction();

	

    
    AbstractAction prevEntryAction = new PrevEntryAction();

	

    
    public StoreFieldAction storeFieldAction;

	

    
    SwitchLeftAction switchLeftAction = new SwitchLeftAction();

	

    SwitchRightAction switchRightAction = new SwitchRightAction();

	

    
    public GenerateKeyAction generateKeyAction;

	

    SaveDatabaseAction saveDatabaseAction = new SaveDatabaseAction();

	

    JPanel mainPanel = new JPanel();

	

    JPanel srcPanel = new JPanel();

	

    EntryEditorTab genPan, optPan, reqPan, absPan;

	

    JTextField bibtexKey;

	

    FieldTextField tf;

	

    JTextArea source;

	

    JToolBar tlb;

	

    JTabbedPane tabbed = new JTabbedPane();

	 

    JLabel lab;

	

    TypeLabel typeLabel;

	

    JabRefFrame frame;

	

    BasePanel panel;

	

    EntryEditor ths = this;

	

    

	

    Logger logger = Logger.getLogger(EntryEditor.class.getName());

	

    boolean updateSource = true;

	 
                                    

    

	

    
    
    boolean lastSourceAccepted = true;

	 

    
    
    
    
    String lastSourceStringAccepted = null;

	 

    
    
    
    private int sourceIndex = -1;

	 

    JabRefPreferences prefs;

	

    HelpAction helpAction;

	

    UndoAction undoAction = new UndoAction();

	

    RedoAction redoAction = new RedoAction();

	

    TabListener tabListener = new TabListener();

	

    public EntryEditor(JabRefFrame frame_, BasePanel panel_, BibtexEntry entry_) {

        frame = frame_;
        panel = panel_;
        entry = entry_;
        prefs = Globals.prefs;
        type = entry.getType();

        entry.addPropertyChangeListener(this);

        helpAction = new HelpAction(frame.helpDiag, GUIGlobals.entryEditorHelp, "Help");
        closeAction = new CloseAction();
        copyKeyAction = new CopyKeyAction();
        generateKeyAction = new GenerateKeyAction(frame);
        storeFieldAction = new StoreFieldAction();

        BorderLayout bl = new BorderLayout();
        setLayout(bl);
        setupToolBar();
        setupFieldPanels();
        setupSourcePanel();
        add(tabbed, BorderLayout.CENTER);
        tabbed.addChangeListener(tabListener);
        if (prefs.getBoolean("showSource") && prefs.getBoolean("defaultShowSource"))
            tabbed.setSelectedIndex(sourceIndex);

        updateAllFields();
    }


	

    private void setupFieldPanels() {
        tabbed.removeAll();
        tabs.clear();
        String[] fields = entry.getRequiredFields();

        List<String> fieldList = null;
        if (fields != null)
            fieldList = java.util.Arrays.asList(fields);
        reqPan = new EntryEditorTab(frame, panel, fieldList, this, true, Globals.lang("Required fields"));
        tabbed.addTab(Globals.lang("Required fields"), GUIGlobals.getImage("required"), reqPan
            .getPane(), Globals.lang("Show required fields"));
        tabs.add(reqPan);

        if ((entry.getOptionalFields() != null) && (entry.getOptionalFields().length >= 1)) {
            optPan = new EntryEditorTab(frame, panel, java.util.Arrays.asList(entry.getOptionalFields()), this,
                false, Globals.lang("Optional fields"));
            tabbed.addTab(Globals.lang("Optional fields"), GUIGlobals.getImage("optional"), optPan
                .getPane(), Globals.lang("Show optional fields"));
            tabs.add(optPan);
        }

        EntryEditorTabList tabList = Globals.prefs.getEntryEditorTabList();
        for (int i = 0; i < tabList.getTabCount(); i++) {
            EntryEditorTab newTab = new EntryEditorTab(frame, panel, tabList.getTabFields(i), this, false,
                tabList.getTabName(i));
            tabbed.addTab(tabList.getTabName(i), GUIGlobals.getImage("general"), newTab.getPane());
            tabs.add(newTab);
        }

        srcPanel.setName(Globals.lang("BibTeX source"));
        if (Globals.prefs.getBoolean("showSource")) {
            tabbed.addTab(Globals.lang("BibTeX source"), GUIGlobals.getImage("source"), srcPanel,
                Globals.lang("Show/edit BibTeX source"));
            tabs.add(srcPanel);
        }
        sourceIndex = tabs.size() - 1; 
        srcPanel.setFocusCycleRoot(true);
    }


	

    public BibtexEntryType getType() {
        return type;
    }


	

    public BibtexEntry getEntry() {
        return entry;
    }


	
    
    public BibtexDatabase getDatabase(){
    	return panel.getDatabase();
    }


	

    private void setupToolBar() {
        tlb = new JToolBar(JToolBar.VERTICAL);

        
        tlb.setMargin(new Insets(0, 0, 0, 2));

        
        
        
        ActionMap am = tlb.getActionMap();
        InputMap im = tlb.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);

        im.put(prefs.getKey("Close entry editor"), "close");
        am.put("close", closeAction);
        im.put(prefs.getKey("Entry editor, store field"), "store");
        am.put("store", storeFieldAction);
        im.put(prefs.getKey("Autogenerate BibTeX keys"), "generateKey");
        am.put("generateKey", generateKeyAction);
        
        im.put(prefs.getKey("Entry editor, previous entry"), "prev");
        am.put("prev", prevEntryAction);
        im.put(prefs.getKey("Entry editor, next entry"), "next");
        am.put("next", nextEntryAction);
        im.put(prefs.getKey("Undo"), "undo");
        am.put("undo", undoAction);
        im.put(prefs.getKey("Redo"), "redo");
        am.put("redo", redoAction);
        im.put(prefs.getKey("Help"), "help");
        am.put("help", helpAction);

        tlb.setFloatable(false);
        tlb.add(closeAction);

        setLabel();
        tlb.add(typeLabel);

        
        
        tlb.addSeparator();
        tlb.add(generateKeyAction);
        tlb.addSeparator();

        
        
        tlb.add(deleteAction);
        tlb.add(prevEntryAction);

        tlb.add(nextEntryAction);
        tlb.addSeparator();
        tlb.add(helpAction);

        Component[] comps = tlb.getComponents();

        for (int i = 0; i < comps.length; i++)
            ((JComponent) comps[i]).setOpaque(false);

        add(tlb, BorderLayout.WEST);
    }


	

    private void setLabel() {
        typeLabel = new TypeLabel(entry.getType().getName());
    }


	

    
    public void rebuildPanels() {
        
        
        tabbed.removeChangeListener(tabListener);
        
        setupFieldPanels();
        
        tabbed.addChangeListener(tabListener);
        revalidate();
        repaint();
    }


	

    
    public JComponent getExtra(String string, final FieldEditor ed) {

        
        final String fieldName = ed.getFieldName();

        String s = BibtexFields.getFieldExtras(string);

        
        if ((fieldName.equals(Globals.prefs.get("timeStampField")))
            || ((s != null) && s.equals("datepicker"))) {
            
            ((JTextArea) ed).addMouseListener(new MouseAdapter() {
                public void mouseClicked(MouseEvent e) {
                    if (e.getClickCount() == 2) 
                    {
                        String date = Util.easyDateFormat();
                        ed.setText(date);
                    }
                }
            });

            
            if ((s != null) && s.equals("datepicker")) {
                DatePickerButton datePicker = new DatePickerButton(ed);
                return datePicker.getDatePicker();
            }
        }

        if ((s != null) && s.equals("external")) {

            
            ((JComponent) ed).addMouseListener(new ExternalViewerListener());

            return null;
        } else if ((s != null) && s.equals("journalNames")) {
            
            
            
            
            JPanel controls = new JPanel();
            controls.setLayout(new BorderLayout());
            if (panel.metaData.getData(Globals.SELECTOR_META_PREFIX + ed.getFieldName()) != null) {
                FieldContentSelector ws = new FieldContentSelector(frame, panel, frame, ed,
                    panel.metaData, storeFieldAction, false, ", ");
                contentSelectors.add(ws);
                controls.add(ws, BorderLayout.NORTH);
            }
            controls.add(JournalAbbreviations.getNameSwitcher(this, ed, panel.undoManager),
                BorderLayout.SOUTH);
            return controls;
        } else if (panel.metaData.getData(Globals.SELECTOR_META_PREFIX + ed.getFieldName()) != null) {
            FieldContentSelector ws = new FieldContentSelector(frame, panel, frame, ed,
                panel.metaData, storeFieldAction, false,
                (ed.getFieldName().equals("author") ? " and " : ", "));
            contentSelectors.add(ws);

            return ws;
        } else if ((s != null) && s.equals("browse")) {
            JButton but = new JButton(Globals.lang("Browse"));
            ((JComponent) ed).addMouseListener(new ExternalViewerListener());

            
            but.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    String dir = ed.getText();

                    if (dir.equals(""))
                        dir = prefs.get(fieldName + Globals.FILETYPE_PREFS_EXT, "");

                    String chosenFile = Globals.getNewFile(frame, new File(dir), "." + fieldName,
                        JFileChooser.OPEN_DIALOG, false);

                    if (chosenFile != null) {
                        File newFile = new File(chosenFile); 
                        ed.setText(newFile.getPath());
                        prefs.put(fieldName + Globals.FILETYPE_PREFS_EXT, newFile.getPath());
                        updateField(ed);
                    }
                }
            });

            return but;
            
        } else if ((s != null) && (s.equals("browseDoc") || s.equals("browseDocZip"))) {

            final String ext = "." + fieldName.toLowerCase();
            final OpenFileFilter off;
            if (s.equals("browseDocZip"))
                off = new OpenFileFilter(new String[] { ext, ext + ".gz", ext + ".bz2" });
            else
                off = new OpenFileFilter(new String[] { ext });

            ExternalFilePanel pan = new ExternalFilePanel(frame, panel.metaData(), this, fieldName,
                off, ed);
            return pan;
        }
        
        else if ((s != null) && s.equals("url")) {
            ((JComponent) ed).setDropTarget(new DropTarget((Component) ed,
                DnDConstants.ACTION_NONE, new SimpleUrlDragDrop(ed, storeFieldAction)));

            return null;
        }

        else if ((s != null) && (s.equals("setOwner"))) {
            JButton button = new JButton(Globals.lang("Auto"));
            button.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent actionEvent) {
                    ed.setText(Globals.prefs.get("defaultOwner"));
                    storeFieldAction.actionPerformed(new ActionEvent(ed, 0, ""));
                }
            });
            return button;
        }
        else
            return null;
    }


	

    private void setupSourcePanel() {
        source = new JTextArea();

        
        
        source.setEditable(true); 
        source.setLineWrap(true);
        source.setTabSize(GUIGlobals.INDENT);
        source.addFocusListener(new FieldEditorFocusListener());
        
        
        
        source.addFocusListener(Globals.focusListener);
        source.setFont(new Font("Monospaced", Font.PLAIN, Globals.prefs.getInt("fontSize")));
        setupJTextComponent(source);
        updateSource();

        JScrollPane sp = new JScrollPane(source, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
            JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        
        
        srcPanel.setLayout(new BorderLayout());
        srcPanel.add(sp, BorderLayout.CENTER);

    }


	

    public void updateSource() {
        if (updateSource) {
            StringWriter sw = new StringWriter(200);

            try {
                entry.write(sw, new net.sf.jabref.export.LatexFieldFormatter(), false);

                String srcString = sw.getBuffer().toString();
                source.setText(srcString);
                lastSourceStringAccepted = srcString;
            } catch (IOException ex) {
                source.setText(ex.getMessage() + "\n\n" + 
                                        Globals.lang("Correct the entry, and "
                    + "reopen editor to display/edit source."));
                source.setEditable(false);
            }


        }
    }


	

    
    public void setupJTextComponent(JTextComponent ta) {


        
        InputMap im = ta.getInputMap(JComponent.WHEN_FOCUSED);
        ActionMap am = ta.getActionMap();

        
        
        im.put(prefs.getKey("Entry editor, store field"), "store");
        am.put("store", storeFieldAction);

        im.put(prefs.getKey("Entry editor, next panel"), "right");
        im.put(prefs.getKey("Entry editor, next panel 2"), "right");
        am.put("right", switchRightAction);

        im.put(prefs.getKey("Entry editor, previous panel"), "left");
        im.put(prefs.getKey("Entry editor, previous panel 2"), "left");
        am.put("left", switchLeftAction);

        im.put(prefs.getKey("Help"), "help");
        am.put("help", helpAction);
        im.put(prefs.getKey("Save database"), "save");
        am.put("save", saveDatabaseAction);

        im.put(Globals.prefs.getKey("Next tab"), "nexttab");
        am.put("nexttab", frame.nextTab);
        im.put(Globals.prefs.getKey("Previous tab"), "prevtab");
        am.put("prevtab", frame.prevTab);
        try {
            HashSet<AWTKeyStroke> keys = new HashSet<AWTKeyStroke>(ta
                .getFocusTraversalKeys(KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS));
            keys.clear();
            keys.add(AWTKeyStroke.getAWTKeyStroke("pressed TAB"));
            ta.setFocusTraversalKeys(KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS, keys);
            keys = new HashSet<AWTKeyStroke>(ta
                .getFocusTraversalKeys(KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS));
            keys.clear();
            keys.add(KeyStroke.getKeyStroke("shift pressed TAB"));
            ta.setFocusTraversalKeys(KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS, keys);
        } catch (Throwable t) {
            System.err.println(t);
        }

        ta.addFocusListener(new FieldListener());
    }


	

    public void requestFocus() {
        activateVisible();
    }


	

    private void activateVisible() {
        Object activeTab = tabs.get(tabbed.getSelectedIndex());

        if (activeTab instanceof EntryEditorTab)
            ((EntryEditorTab) activeTab).activate();
        else
            new FocusRequester(source);
        
    }


	

    
    public boolean isEnabled() {
        return source.isEnabled();
    }


	

    
    public void setEnabled(boolean enabled) {
        for (Iterator<Object> i = tabs.iterator(); i.hasNext();) {
            Object o = i.next();
            if (o instanceof EntryEditorTab) {
                ((EntryEditorTab) o).setEnabled(enabled);
            }
        }
        source.setEnabled(enabled);

    }


	

    
    private void scrollTo(int row) {
        panel.mainTable.setRowSelectionInterval(row, row);
        panel.mainTable.ensureVisible(row);
    }


	

    
    public void storeCurrentEdit() {
        Component comp = Globals.focusListener.getFocused();
        if ((comp instanceof FieldEditor) && this.isAncestorOf(comp)) {
            storeFieldAction.actionPerformed(new ActionEvent(comp, 0, ""));
        }
    }


	

    
    public int getVisiblePanel() {
        return tabbed.getSelectedIndex();
    }


	

    
    public String getVisiblePanelName() {
        return tabbed.getSelectedComponent().getName();
    }


	

    
    public void setVisiblePanel(int i) {
        tabbed.setSelectedIndex(Math.min(i, tabbed.getTabCount() - 1));
    }


	

    public void setVisiblePanel(String name) {
        for (int i = 0; i < tabbed.getTabCount(); ++i) {
            if (name.equals(tabbed.getComponent(i).getName())) {
                tabbed.setSelectedIndex(i);
                return;
            }
        }
        if (tabbed.getTabCount() > 0)
            tabbed.setSelectedIndex(0);
    }


	

    
    public synchronized void switchTo(BibtexEntry be) {
        if (entry == be)
            return;

        
        
        storeCurrentEdit();

        
        entry.removePropertyChangeListener(this);

        
        be.addPropertyChangeListener(this);

        entry = be;

        updateAllFields();
        validateAllFields();
        updateSource();
        panel.showing = be;

    }


	

    
    public boolean lastSourceAccepted() {
        if (tabbed.getSelectedComponent() == srcPanel)
            storeSource(false);

        return lastSourceAccepted;
    }


	

    
    public boolean storeSource(boolean showError) {
        
        BibtexParser bp = new BibtexParser(new java.io.StringReader(source.getText()));

        try {
            BibtexDatabase db = bp.parse().getDatabase();

            if (db.getEntryCount() > 1)
                throw new Exception("More than one entry found.");

            if (db.getEntryCount() < 1)
                throw new Exception("No entries found.");

            NamedCompound compound = new NamedCompound(Globals.lang("source edit"));
            BibtexEntry nu = db.getEntryById((String) db.getKeySet().iterator().next());
            String id = entry.getId();
            String
            
            newKey = nu.getCiteKey();
            boolean anyChanged = false;
            boolean duplicateWarning = false;
            boolean emptyWarning = newKey == null || newKey.equals("");

            if (panel.database.setCiteKeyForEntry(id, newKey)) {
                duplicateWarning = true;

                
            }

            for (String field : entry.getAllFields()){
                if (BibtexFields.isDisplayableField(field.toString())) {
                    if (nu.getField(field.toString()) == null) {
                        compound.addEdit(new UndoableFieldChange(entry, field.toString(), entry
                            .getField(field.toString()), null));
                        entry.clearField(field.toString());
                        anyChanged = true;
                    }
                }
            }

            
            for (String field : nu.getAllFields()){
                if (entry.getField(field.toString()) != nu.getField(field.toString())) {
                    String toSet = (String) nu.getField(field.toString());

                    
                    (new LatexFieldFormatter()).format(toSet, field.toString());

                    compound.addEdit(new UndoableFieldChange(entry, field.toString(), entry
                        .getField(field.toString()), toSet));
                    entry.setField(field.toString(), toSet);
                    anyChanged = true;
                }
            }

            compound.end();

            if (!anyChanged)
                return true;

            panel.undoManager.addEdit(compound);

            
            if (duplicateWarning) {
                warnDuplicateBibtexkey();
            } else if (emptyWarning && showError) {
                warnEmptyBibtexkey();
            } else {
                panel.output(Globals.lang("Stored entry") + ".");
            }

            lastSourceStringAccepted = source.getText();
            updateAllFields();
            lastSourceAccepted = true;
            updateSource = true;

            
            
            
            
            panel.markBaseChanged();

            return true;
        } catch (Throwable ex) {
            
            
            
            
            updateSource = false;
            lastSourceAccepted = false;
            tabbed.setSelectedComponent(srcPanel);

            if (showError) {
                Object[] options = { Globals.lang("Edit"),
                    Globals.lang("Revert to original source") };

                int answer = JOptionPane.showOptionDialog(frame, Globals.lang("Error: ") + ex.getMessage(),
                    Globals.lang("Problem with parsing entry"), JOptionPane.YES_NO_OPTION,
                    JOptionPane.ERROR_MESSAGE, null, options, options[0]);

                if (answer != 0) {
                    updateSource = true;
                    updateSource();
                }
            }

            return false;
        }
    }


	

    public void setField(String fieldName, String newFieldData) {

        for (Iterator<Object> i = tabs.iterator(); i.hasNext();) {
            Object o = i.next();
            if (o instanceof EntryEditorTab) {
                ((EntryEditorTab) o).updateField(fieldName, newFieldData);
            }
        }

    }


	

    
    public void updateAllFields() {
        for (Iterator<Object> i = tabs.iterator(); i.hasNext();) {
            Object o = i.next();
            if (o instanceof EntryEditorTab) {
                ((EntryEditorTab) o).setEntry(entry);
            }
        }
    }


	

    
    public void validateAllFields() {
        for (Iterator<Object> i = tabs.iterator(); i.hasNext();) {
            Object o = i.next();
            if (o instanceof EntryEditorTab) {
                ((EntryEditorTab) o).validateAllFields();
            }
        }
    }


	

    public void updateAllContentSelectors() {
        if (contentSelectors.size() > 0) {
            for (Iterator<FieldContentSelector> i = contentSelectors.iterator(); i.hasNext();)
                i.next().rebuildComboBox();
        }
    }


	

    
    public void vetoableChange(PropertyChangeEvent e) {
        String newValue = ((e.getNewValue() != null) ? e.getNewValue().toString() : "");
        setField(e.getPropertyName(), newValue);
    }


	

    public void updateField(final Object source) {
        storeFieldAction.actionPerformed(new ActionEvent(source, 0, ""));
    }


	

    private  class  TypeLabel  extends JPanel {
		
        private String label;

		

        public TypeLabel(String type) {
            label = type;
            addMouseListener(new MouseAdapter() {
                public void mouseClicked(MouseEvent e) {
                    boolean ctrlClick = prefs.getBoolean("ctrlClick");

                    if ((e.getButton() == MouseEvent.BUTTON3)
                        || (ctrlClick && (e.getButton() == MouseEvent.BUTTON1) && e.isControlDown())) {
                        JPopupMenu typeMenu = new JPopupMenu();

                        
                        for (String s: BibtexEntryType.ALL_TYPES.keySet())
                            typeMenu.add(new ChangeTypeAction(BibtexEntryType.getType(s), panel));

                        typeMenu.show(ths, e.getX(), e.getY());
                    }
                }
            });
        }


		

        public void paint(Graphics g) {
            Graphics2D g2 = (Graphics2D) g;
            g2.setColor(GUIGlobals.validFieldColor);
            g2.setFont(GUIGlobals.typeNameFont);

            FontMetrics fm = g2.getFontMetrics();
            int width = fm.stringWidth(label);
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.rotate(-Math.PI / 2, 0, 0);
            g2.drawString(label, -width - 7, 28);
        }



	}

	

     

    class  FieldListener  extends FocusAdapter {
		
        
        public void focusGained(FocusEvent e) {
        }


		

        public void focusLost(FocusEvent e) {
            
            if (!e.isTemporary())
                updateField(e.getSource());
        }



	}

	

     

    class  TabListener implements  ChangeListener {
		
        public void stateChanged(ChangeEvent e) {

            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    activateVisible();
                }
            });

            
            
            
            
            
            SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    Object activeTab = tabs.get(tabbed.getSelectedIndex());
                    if (activeTab instanceof EntryEditorTab)
                        ((EntryEditorTab) activeTab).updateAll();
                }
            });

        }



	}

	

     

    class  DeleteAction  extends AbstractAction {
		
        public DeleteAction() {
            super(Globals.lang("Delete"), GUIGlobals.getImage("delete"));
            putValue(SHORT_DESCRIPTION, Globals.lang("Delete entry"));
        }


		

        public void actionPerformed(ActionEvent e) {
            
            boolean goOn = panel.showDeleteConfirmationDialog(1);

            if (!goOn)
                return;

            panel.entryEditorClosing(EntryEditor.this);
            panel.database.removeEntry(entry.getId());
            panel.markBaseChanged();
            panel.undoManager.addEdit(new UndoableRemoveEntry(panel.database, entry, panel));
            panel.output(Globals.lang("Deleted") + " " + Globals.lang("entry"));
        }



	}

	

     

    class  CloseAction  extends AbstractAction {
		
        public CloseAction() {
            super(Globals.lang("Close window"), GUIGlobals.getImage("close"));
            putValue(SHORT_DESCRIPTION, Globals.lang("Close window"));
        }


		

        public void actionPerformed(ActionEvent e) {
            if (tabbed.getSelectedComponent() == srcPanel) {
                updateField(source);
                if (lastSourceAccepted)
                    panel.entryEditorClosing(EntryEditor.this);
            } else
                panel.entryEditorClosing(EntryEditor.this);
        }



	}

	

     

    class  CopyKeyAction  extends AbstractAction {
		
        public CopyKeyAction() {
            super("Copy BibTeX key to clipboard");
            putValue(SHORT_DESCRIPTION, "Copy BibTeX key to clipboard (Ctrl-K)");
            
        }


		

        public void actionPerformed(ActionEvent e) {
            String s = (String) (entry.getField(BibtexFields.KEY_FIELD));
            StringSelection ss = new StringSelection(s);

            if (s != null)
                Toolkit.getDefaultToolkit().getSystemClipboard().setContents(ss, ss);
        }



	}

	

    public  class  StoreFieldAction  extends AbstractAction {
		
        public StoreFieldAction() {
            super("Store field value");
            putValue(SHORT_DESCRIPTION, "Store field value");
        }


		

        public void actionPerformed(ActionEvent e) {

            if (e.getSource() instanceof FieldTextField) {
                
                FieldTextField fe = (FieldTextField) e.getSource();
                String oldValue = entry.getCiteKey();
                String newValue = fe.getText();

                if (newValue.equals(""))
                    newValue = null;

                if (((oldValue == null) && (newValue == null))
                    || ((oldValue != null) && (newValue != null) && oldValue.equals(newValue)))
                    return; 

                
                String cleaned = Util.checkLegalKey(newValue);
                if ((cleaned != null) && !cleaned.equals(newValue)) {
                    JOptionPane.showMessageDialog(frame, Globals.lang("Invalid BibTeX key"),
                        Globals.lang("Error setting field"), JOptionPane.ERROR_MESSAGE);
                    fe.setBackground(GUIGlobals.invalidFieldBackground);
                    return;
                } else {
                    fe.setBackground(
                    GUIGlobals.validFieldBackground);
                }

                boolean isDuplicate = panel.database.setCiteKeyForEntry(entry.getId(), newValue);

                if (newValue != null) {
                    if (isDuplicate)
                        warnDuplicateBibtexkey();
                    else
                        panel.output(Globals.lang("BibTeX key is unique."));
                } else { 
                    warnEmptyBibtexkey();
                }

                
                panel.undoManager.addEdit(new UndoableKeyChange(panel.database, entry.getId(),
                    oldValue, newValue));

                if ((newValue != null) && (newValue.length() > 0))
                    
                    fe.setBackground(GUIGlobals.validFieldBackground);
                else
                    
                    fe.setBackground(GUIGlobals.validFieldBackground);

                updateSource();
                panel.markBaseChanged();
            }
            else if (e.getSource() instanceof FieldEditor) {
                String toSet = null;
                FieldEditor fe = (FieldEditor) e.getSource();
                boolean set;
                
                String currentText = fe.getText();
                String trim = currentText.trim();
                if (trim.length() > 0) {
                    toSet = trim;
                }

                
                
                if (toSet == null) {
                    if (entry.getField(fe.getFieldName()) == null)
                        set = false;
                    else
                        set = true;
                } else {
                    if ((entry.getField(fe.getFieldName()) != null)
                        && toSet.equals(entry.getField(fe.getFieldName()).toString()))
                        set = false;
                    else
                        set = true;
                }

                if (set) {
                    try {
                        
                        
                        
                        
                        
                        if (toSet != null)
                            (new LatexFieldFormatter()).format(toSet, fe.getFieldName());

                        String oldValue = entry.getField(fe.getFieldName());

                        if (toSet != null)
                            entry.setField(fe.getFieldName(), toSet);
                        else
                            entry.clearField(fe.getFieldName());

                        if ((toSet != null) && (toSet.length() > 0))
                            
                            fe.setBackground(GUIGlobals.validFieldBackground);
                        else
                            
                            fe.setBackground(GUIGlobals.validFieldBackground);

                        
                        AutoCompleter aComp = panel.getAutoCompleter(fe.getFieldName());
                        if (aComp != null)
                            aComp.addAll(toSet);

                        
                        
                        panel.undoManager.addEdit(new UndoableFieldChange(entry, fe.getFieldName(),
                            oldValue, toSet));
                        updateSource();
                        panel.markBaseChanged();

                        
                        
                        SwingUtilities.invokeLater(new Runnable() {
                            public void run() {
                                panel.highlightEntry(entry);
                            }
                        });

                    } catch (IllegalArgumentException ex) {
                        JOptionPane.showMessageDialog(frame, Globals.lang("Error: ") + ex.getMessage(), Globals
                            .lang("Error setting field"), JOptionPane.ERROR_MESSAGE);
                        fe.setBackground(GUIGlobals.invalidFieldBackground);
                    }
                } else {
                    
                    
                    fe.setBackground(GUIGlobals.validFieldBackground);
                }
            } else if ((source.isEditable())
                && (!source.getText().equals(lastSourceStringAccepted))) {
                boolean accepted = storeSource(true);

                if (accepted) {
                }
            }
        }



	}

	

     

    class  SwitchLeftAction  extends AbstractAction {
		
        public SwitchLeftAction() {
            super("Switch to the panel to the left");
        }


		

        public void actionPerformed(ActionEvent e) {
            
            int i = tabbed.getSelectedIndex();
            tabbed.setSelectedIndex(((i > 0) ? (i - 1) : (tabbed.getTabCount() - 1)));

            activateVisible();
        }



	}

	

     

    class  SwitchRightAction  extends AbstractAction {
		
        public SwitchRightAction() {
            super("Switch to the panel to the right");
        }


		

        public void actionPerformed(ActionEvent e) {
            
            int i = tabbed.getSelectedIndex();
            tabbed.setSelectedIndex((i < (tabbed.getTabCount() - 1)) ? (i + 1) : 0);
            activateVisible();

        }



	}

	

     

    class  NextEntryAction  extends AbstractAction {
		
        public NextEntryAction() {
            super(Globals.lang("Next entry"), GUIGlobals.getImage("down"));

            putValue(SHORT_DESCRIPTION, Globals.lang("Next entry"));
        }


		

        public void actionPerformed(ActionEvent e) {

            int thisRow = panel.mainTable.findEntry(entry);
            int newRow = -1;

            if ((thisRow + 1) < panel.database.getEntryCount())
                newRow = thisRow + 1;
            else if (thisRow > 0)
                newRow = 0;
            else
                return; 
                        

            scrollTo(newRow);
            panel.mainTable.setRowSelectionInterval(newRow, newRow);

        }



	}

	

     

    class  PrevEntryAction  extends AbstractAction {
		
        public PrevEntryAction() {
            super(Globals.lang("Previous entry"), GUIGlobals.getImage("up"));

            putValue(SHORT_DESCRIPTION, Globals.lang("Previous entry"));
        }


		

        public void actionPerformed(ActionEvent e) {
            int thisRow = panel.mainTable.findEntry(entry);
            int newRow = -1;

            if ((thisRow - 1) >= 0)
                newRow = thisRow - 1;
            else if (thisRow != (panel.database.getEntryCount() - 1))
                newRow = panel.database.getEntryCount() - 1;
            else
                return; 
                        
            
            

            scrollTo(newRow);
            panel.mainTable.setRowSelectionInterval(newRow, newRow);

        }



	}

	

     

    class  GenerateKeyAction  extends AbstractAction {
		
        JabRefFrame parent;

		

        BibtexEntry selectedEntry;

		

        public GenerateKeyAction(JabRefFrame parentFrame) {
            super(Globals.lang("Generate BibTeX key"), GUIGlobals.getImage("makeKey"));
            parent = parentFrame;

            
            putValue(SHORT_DESCRIPTION, Globals.lang("Generate BibTeX key"));

            
        }


		

        public void actionPerformed(ActionEvent e) {
            
            
            try {
                
                
                Object oldValue = entry.getField(BibtexFields.KEY_FIELD);

                
                LabelPatternUtil.makeLabel(prefs.getKeyPattern(), panel.database, entry);

                
                panel.undoManager.addEdit(new UndoableKeyChange(panel.database, entry.getId(),
                    (String) oldValue, (String) entry.getField(BibtexFields.KEY_FIELD)));

                
                String bibtexKeyData = (String) entry.getField(BibtexFields.KEY_FIELD);

                
                setField(BibtexFields.KEY_FIELD, bibtexKeyData);
                updateSource();
                panel.markBaseChanged();
            } catch (Throwable t) {
                System.err.println("error setting key: " + t);
            }
        }



	}

	

     

    class  UndoAction  extends AbstractAction {
		
        public UndoAction() {
            super("Undo", GUIGlobals.getImage("undo"));
            putValue(SHORT_DESCRIPTION, "Undo");
        }


		

        public void actionPerformed(ActionEvent e) {
            try {
                panel.runCommand("undo");
            } catch (Throwable ex) {
            }
        }



	}

	

     

    class  RedoAction  extends AbstractAction {
		
        public RedoAction() {
            super("Undo", GUIGlobals.getImage("redo"));
            putValue(SHORT_DESCRIPTION, "Redo");
        }


		

        public void actionPerformed(ActionEvent e) {
            try {
                panel.runCommand("redo");
            } catch (Throwable ex) {
            }
        }



	}

	

     

    class  SaveDatabaseAction  extends AbstractAction {
		
        public SaveDatabaseAction() {
            super("Save database");
        }


		

        public void actionPerformed(ActionEvent e) {
            Object activeTab = tabs.get(tabbed.getSelectedIndex());
            if (activeTab instanceof EntryEditorTab) {
                
                EntryEditorTab fp = (EntryEditorTab) activeTab;
                updateField(fp.getActive());
            } else
                
                updateField(activeTab);

            try {
                panel.runCommand("save");
            } catch (Throwable ex) {
            }
        }



	}

	

     

    class  ExternalViewerListener  extends MouseAdapter {
		
        public void mouseClicked(MouseEvent evt) {
            if (evt.getClickCount() == 2) {
                FieldTextArea tf = (FieldTextArea) evt.getSource();

                if (tf.getText().equals(""))
                    return;

                tf.selectAll();

                String link = tf.getText(); 

                
                try {
                    Util.openExternalViewer(panel.metaData(), link, tf.getFieldName());
                } catch (IOException ex) {
                    System.err.println("Error opening file.");
                }
            }
        }



	}

	

     

    class  ChangeTypeAction  extends AbstractAction {
		
        BibtexEntryType type;

		

        BasePanel panel;

		

        public ChangeTypeAction(BibtexEntryType type, BasePanel bp) {
            super(type.getName());
            this.type = type;
            panel = bp;
        }


		

        public void actionPerformed(ActionEvent evt) {
            panel.changeType(entry, type);
        }



	}

	

    
    


	

    private void warnDuplicateBibtexkey() {
        panel.output(Globals.lang("Warning") + ": " + Globals.lang("Duplicate BibTeX key."));

        if (prefs.getBoolean("dialogWarningForDuplicateKey")) {
            
            CheckBoxMessage jcb = new CheckBoxMessage(Globals.lang("Warning") + ": "
                + Globals.lang("Duplicate BibTeX key. Grouping may not work for this entry."),
                Globals.lang("Disable this warning dialog"), false);
            JOptionPane.showMessageDialog(frame, jcb, Globals.lang("Warning"),
                JOptionPane.WARNING_MESSAGE);

            if (jcb.isSelected())
                prefs.putBoolean("dialogWarningForDuplicateKey", false);
        }
    }


	

    private void warnEmptyBibtexkey() {
        
        panel.output(Globals.lang("Warning") + ": " + Globals.lang("Empty BibTeX key."));

        if (prefs.getBoolean("dialogWarningForEmptyKey")) {
            
            CheckBoxMessage jcb = new CheckBoxMessage(Globals.lang("Warning") + ": "
                + Globals.lang("Empty BibTeX key. Grouping may not work for this entry."), Globals
                .lang("Disable this warning dialog"), false);
            JOptionPane.showMessageDialog(frame, jcb, Globals.lang("Warning"),
                JOptionPane.WARNING_MESSAGE);

            if (jcb.isSelected())
                prefs.putBoolean("dialogWarningForEmptyKey", false);
        }
    }


	

    HashSet<FieldContentSelector> contentSelectors = new HashSet<FieldContentSelector>();

	 
                                    

    List<Object> tabs = new ArrayList<Object>();


}
