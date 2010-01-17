

package net.sf.jabref;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import net.sf.jabref.export.ExpandEndnoteFilters;
import net.sf.jabref.export.ExportCustomizationDialog;
import net.sf.jabref.export.ExportFormats;
import net.sf.jabref.export.SaveAllAction;
import net.sf.jabref.external.ExternalFileTypeEditor;
import net.sf.jabref.external.PushToApplicationButton;
import net.sf.jabref.groups.EntryTableTransferHandler;
import net.sf.jabref.groups.GroupSelector;
import net.sf.jabref.gui.*;
import net.sf.jabref.imports.*;
import net.sf.jabref.journals.ManageJournalsAction;
import net.sf.jabref.label.*;
import net.sf.jabref.plugin.PluginCore;
import net.sf.jabref.plugin.core.JabRefPlugin;
import net.sf.jabref.plugin.core.generated._JabRefPlugin.EntryFetcherExtension;
import net.sf.jabref.undo.NamedCompound;
import net.sf.jabref.undo.UndoableInsertEntry;
import net.sf.jabref.undo.UndoableRemoveEntry;
import net.sf.jabref.util.MassSetFieldAction;
import net.sf.jabref.wizard.auximport.gui.FromAuxDialog;
import net.sf.jabref.wizard.integrity.gui.IntegrityWizard;

import com.jgoodies.looks.HeaderStyle;
import com.jgoodies.looks.Options;
import com.jgoodies.uif_lite.component.UIFSplitPane;


public class JabRefFrame extends JFrame {

    
    JabRefFrame ths = this;
    UIFSplitPane contentPane = new UIFSplitPane();

    JabRefPreferences prefs = Globals.prefs; 
    PrefsDialog3 prefsDialog = null;
    
    private int lastTabbedPanelSelectionIndex = -1 ;

    
    public SidePaneManager sidePaneManager;

    JTabbedPane tabbedPane = new JTabbedPane();
    
    final Insets marg = new Insets(1,0,2,0);

    class ToolBar extends JToolBar {
      void addAction(Action a) {
        JButton b = new JButton(a);
        b.setText(null);
        if (!Globals.ON_MAC)
            b.setMargin(marg);
        add(b);
      }
    }
    ToolBar tlb = new ToolBar();

    JMenuBar mb = new JMenuBar();

    GridBagLayout gbl = new GridBagLayout();

    GridBagConstraints con = new GridBagConstraints();

    JLabel statusLine = new JLabel("", SwingConstants.LEFT), statusLabel = new JLabel(Globals
        .lang("Status")
        + ":", SwingConstants.LEFT);
    JProgressBar progressBar = new JProgressBar();

    

    private FileHistory fileHistory = new FileHistory(prefs, this);

    LabelMaker labelMaker;

    
    public HelpDialog helpDiag = new HelpDialog(this);

    
    
    
    
    
    

  
  public JToggleButton groupToggle, searchToggle, previewToggle, highlightAny,
      highlightAll;

  OpenDatabaseAction
      open = new OpenDatabaseAction(this, true);
  AbstractAction
      close = new CloseDatabaseAction(),
      quit = new CloseAction(),
      selectKeys = new SelectKeysAction(),
      newDatabaseAction = new NewDatabaseAction(),
      newSubDatabaseAction = new NewSubDatabaseAction(),
      integrityCheckAction = new IntegrityCheckAction(),
      help = new HelpAction("JabRef help", helpDiag,
                            GUIGlobals.baseFrameHelp, Globals.lang("JabRef help"),
                            prefs.getKey("Help")),
      contents = new HelpAction("Help contents", helpDiag,
                                GUIGlobals.helpContents, Globals.lang("Help contents"),
                                GUIGlobals.getIconUrl("helpContents")),
      about = new HelpAction("About JabRef", helpDiag,
                             GUIGlobals.aboutPage, Globals.lang("About JabRef"),
                             GUIGlobals.getIconUrl("about")),
      editEntry = new GeneralAction("edit", "Edit entry",
                               Globals.lang("Edit entry"),
                               prefs.getKey("Edit entry")),
      save = new GeneralAction("save", "Save database",
                               Globals.lang("Save database"),
                               prefs.getKey("Save database")),
      saveAs = new GeneralAction("saveAs", "Save database as ...",
                                 Globals.lang("Save database as ..."),
                                 prefs.getKey("Save database as ...")),
      saveAll = new SaveAllAction(ths),
      saveSelectedAs = new GeneralAction("saveSelectedAs",
                                         "Save selected as ...",
                                         Globals.lang("Save selected as ..."),
                                         GUIGlobals.getIconUrl("saveAs")),
      exportAll = ExportFormats.getExportAction(this, false),
      exportSelected = ExportFormats.getExportAction(this, true),
      importCurrent = ImportFormats.getImportAction(this, false),
      importNew = ImportFormats.getImportAction(this, true),
      nextTab = new ChangeTabAction(true),
      prevTab = new ChangeTabAction(false),
      sortTabs = new SortTabsAction(this),
      undo = new GeneralAction("undo", "Undo", Globals.lang("Undo"),
                               prefs.getKey("Undo")),
      redo = new GeneralAction("redo", "Redo", Globals.lang("Redo"),
                               prefs.getKey("Redo")),
      
      delete = new GeneralAction("delete", "Delete", Globals.lang("Delete"),
                                 prefs.getKey("Delete")),
      
      copy = new EditAction("copy", GUIGlobals.getIconUrl("copy")),
      paste = new EditAction("paste", GUIGlobals.getIconUrl("paste")),
      cut = new EditAction("cut", GUIGlobals.getIconUrl("cut")),
      mark = new GeneralAction("markEntries", "Mark entries",
                               Globals.lang("Mark entries"),
                               prefs.getKey("Mark entries")),
       unmark = new GeneralAction("unmarkEntries", "Unmark entries",
                                  Globals.lang("Unmark entries"),
                                  prefs.getKey("Unmark entries")),
       unmarkAll = new GeneralAction("unmarkAll", "Unmark all"),
      manageSelectors = new GeneralAction("manageSelectors", "Manage content selectors"),
      saveSessionAction = new SaveSessionAction(),
      loadSessionAction = new LoadSessionAction(),
      incrementalSearch = new GeneralAction("incSearch", "Incremental search",
                                            Globals.lang("Start incremental search"),
                                            prefs.getKey("Incremental search")),
      normalSearch = new GeneralAction("search", "Search", Globals.lang("Search"),
                                       prefs.getKey("Search")),
      toggleSearch = new GeneralAction("toggleSearch", "Search", Globals.lang("Toggle search panel")),

      fetchCiteSeer = new FetchCiteSeerAction(),
      importCiteSeer = new ImportCiteSeerAction(),
      fetchMedline = new FetchMedlineAction(),
      citeSeerPanelAction = new CiteSeerPanelAction(),
      
      copyKey = new GeneralAction("copyKey", "Copy BibTeX key"),
      
      copyCiteKey = new GeneralAction("copyCiteKey", "Copy \\cite{BibTeX key}",
                                      
                                      prefs.getKey("Copy \\cite{BibTeX key}")),
      mergeDatabaseAction = new GeneralAction("mergeDatabase",
                                              "Append database",
                                              Globals.lang("Append contents from a BibTeX database into the currently viewed database"),
                                              GUIGlobals.getIconUrl("open")),
      
      
      selectAll = new GeneralAction("selectAll", "Select all",
                                    prefs.getKey("Select all")),
      replaceAll = new GeneralAction("replaceAll", "Replace string",
                                     prefs.getKey("Replace string")),

      editPreamble = new GeneralAction("editPreamble", "Edit preamble",
                                       Globals.lang("Edit preamble"),
                                       prefs.getKey("Edit preamble")),
      editStrings = new GeneralAction("editStrings", "Edit strings",
                                      Globals.lang("Edit strings"),
                                      prefs.getKey("Edit strings")),
      toggleGroups = new GeneralAction("toggleGroups",
                                       "Toggle groups interface",
                                       Globals.lang("Toggle groups interface"),
                                       prefs.getKey("Toggle groups interface")),
      togglePreview = new GeneralAction("togglePreview",
                                        "Toggle entry preview",
                                        Globals.lang("Toggle entry preview"),
                                        prefs.getKey("Toggle entry preview")),
      toggleHighlightAny = new GeneralAction("toggleHighlightGroupsMatchingAny",
                                        "Highlight groups matching any selected entry",
                                        Globals.lang("Highlight groups matching any selected entry"),
                                        GUIGlobals.getIconUrl("groupsHighlightAny")),
      toggleHighlightAll = new GeneralAction("toggleHighlightGroupsMatchingAll",
                                        "Highlight groups matching all selected entries",
                                        Globals.lang("Highlight groups matching all selected entries"),
                                        GUIGlobals.getIconUrl("groupsHighlightAll")),
      switchPreview = new GeneralAction("switchPreview",
                                        "Switch preview layout",
                                        prefs.getKey("Switch preview layout")),
       makeKeyAction = new GeneralAction("makeKey", "Autogenerate BibTeX keys",
                                        Globals.lang("Autogenerate BibTeX keys"),
                                        prefs.getKey("Autogenerate BibTeX keys")),


      writeXmpAction = new GeneralAction("writeXMP", "Write XMP-metadata to PDFs",
                                        Globals.lang("Will write XMP-metadata to the PDFs linked from selected entries."),
                                        prefs.getKey("Write XMP")),
      
      openFile = new GeneralAction("openFile", "Open PDF or PS",
                                   Globals.lang("Open PDF or PS"),
                                   prefs.getKey("Open PDF or PS")),
      openUrl = new GeneralAction("openUrl", "Open URL or DOI",
                                  Globals.lang("Open URL or DOI"),
                                  prefs.getKey("Open URL or DOI")),
      dupliCheck = new GeneralAction("dupliCheck", "Find duplicates"),
      
      plainTextImport = new GeneralAction("plainTextImport",
                                          "New entry from plain text",
                                          prefs.getKey("New from plain text")),


      customExpAction = new CustomizeExportsAction(),
      customImpAction = new CustomizeImportsAction(),
      customFileTypesAction = ExternalFileTypeEditor.getAction(this),
      exportToClipboard = new GeneralAction("exportToClipboard", "Export selected entries to clipboard"),
      expandEndnoteZip = new ExpandEndnoteFilters(this),
        autoSetPdf = new GeneralAction("autoSetPdf", Globals.lang("Synchronize %0 links", "PDF"), Globals.prefs.getKey("Synchronize PDF")),
        autoSetPs = new GeneralAction("autoSetPs", Globals.lang("Synchronize %0 links", "PS"), Globals.prefs.getKey("Synchronize PS")),
        autoSetFile = new GeneralAction("autoSetFile", Globals.lang("Synchronize file links"), Globals.prefs.getKey("Synchronize files")),

    abbreviateMedline = new GeneralAction("abbreviateMedline", "Abbreviate journal names (MEDLINE)",
                Globals.lang("Abbreviate journal names of the selected entries (MEDLINE abbreviation)")),
  abbreviateIso = new GeneralAction("abbreviateIso", "Abbreviate journal names (ISO)",
                          Globals.lang("Abbreviate journal names of the selected entries (ISO abbreviation)"),
                          Globals.prefs.getKey("Abbreviate")),


    unabbreviate = new GeneralAction("unabbreviate", "Unabbreviate journal names",
                    Globals.lang("Unabbreviate journal names of the selected entries"),
            Globals.prefs.getKey("Unabbreviate")),
    manageJournals = new ManageJournalsAction(this),
    databaseProperties = new DatabasePropertiesAction(),
    upgradeExternalLinks = new GeneralAction("upgradeLinks", "Upgrade external links",
            Globals.lang("Upgrade external PDF/PS links to use the '%0' field.", GUIGlobals.FILE_FIELD)),
      errorConsole = Globals.errorConsole.getAction(this),
    test = new GeneralAction("test", "Test");

    PushToApplicationButton pushExternalButton;
  


    MedlineFetcher medlineFetcher;
    CiteSeerFetcher citeSeerFetcher;
    CiteSeerFetcherPanel citeSeerFetcherPanel;
    
    List<EntryFetcher> fetchers = new LinkedList<EntryFetcher>();
    List<Action> fetcherActions = new LinkedList<Action>();

    SearchManager2 searchManager;
    public GroupSelector groupSelector;

  
  JMenu importMenu = subMenu("Import into current database"),
      importNewMenu = subMenu("Import into new database"),
      exportMenu = subMenu("Export"),
      customExportMenu = subMenu("Custom export"),
      newDatabaseMenu = subMenu("New database" );

  
  JMenu checkAndFix = subMenu("Scan database...");


  
  NewEntryAction newEntryAction = new NewEntryAction(prefs.getKey("New entry"));
  NewEntryAction[] newSpecificEntryAction = new NewEntryAction[]
  {
      new NewEntryAction("article", prefs.getKey("New article")),
      new NewEntryAction("book", prefs.getKey("New book")),
      new NewEntryAction("phdthesis", prefs.getKey("New phdthesis")),
      new NewEntryAction("inbook", prefs.getKey("New inbook")),
      new NewEntryAction("mastersthesis", prefs.getKey("New mastersthesis")),
      new NewEntryAction("proceedings", prefs.getKey("New proceedings")),
      new NewEntryAction("inproceedings"),
      new NewEntryAction("conference"),
      new NewEntryAction("incollection"),
      new NewEntryAction("booklet"),
      new NewEntryAction("manual"),
      new NewEntryAction("techreport"),
      new NewEntryAction("unpublished",
                         prefs.getKey("New unpublished")),
      new NewEntryAction("misc"),
      new NewEntryAction("other")
  };

  public JabRefFrame() {
    init();
    updateEnabledState();
  }

  private void init() {

        macOSXRegistration();
        MyGlassPane glassPane = new MyGlassPane();
        setGlassPane(glassPane);
        

        setTitle(GUIGlobals.frameTitle);
        setIconImage(GUIGlobals.getImage("jabrefIcon").getImage());
        setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent e) {
                (new CloseAction()).actionPerformed(null);
            }
        });

        initLabelMaker();

        initSidePane();
        
        initLayout();
        
        initActions();
        
        if (Globals.prefs.getBoolean("rememberWindowLocation")) {
            setSize(new Dimension(prefs.getInt("sizeX"), prefs.getInt("sizeY")));
            setLocation(new Point(prefs.getInt("posX"), prefs.getInt("posY")));
        }
        tabbedPane.setBorder(null);
        tabbedPane.setForeground(GUIGlobals.inActiveTabbed);

        
        tabbedPane.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent e) {
                markActiveBasePanel();

                BasePanel bp = basePanel();
                if (bp != null) {
                    groupToggle.setSelected(sidePaneManager.isComponentVisible("groups"));
                    searchToggle.setSelected(sidePaneManager.isComponentVisible("search"));
                    previewToggle.setSelected(Globals.prefs.getBoolean("previewEnabled"));
                    highlightAny
                        .setSelected(Globals.prefs.getBoolean("highlightGroupsMatchingAny"));
                    highlightAll
                        .setSelected(Globals.prefs.getBoolean("highlightGroupsMatchingAll"));
                    Globals.focusListener.setFocused(bp.mainTable);

                    new FocusRequester(bp.mainTable);
                }
            }
        });
    }



    private void initSidePane() {
        sidePaneManager = new SidePaneManager(this);

        Globals.sidePaneManager = this.sidePaneManager;
        Globals.helpDiag = this.helpDiag;

        
        JabRefPlugin jabrefPlugin = JabRefPlugin.getInstance(PluginCore.getManager());
    	if (jabrefPlugin != null){
    		for (EntryFetcherExtension ext : jabrefPlugin.getEntryFetcherExtensions()){
    			EntryFetcher fetcher = ext.getEntryFetcher();
    			if (fetcher != null){
    				fetchers.add(fetcher);
    			}
    		}
    	}
        
        medlineFetcher = new MedlineFetcher(sidePaneManager);
        citeSeerFetcher = new CiteSeerFetcher(sidePaneManager);
        citeSeerFetcherPanel = new CiteSeerFetcherPanel(sidePaneManager,
            (CiteSeerFetcher) citeSeerFetcher);
        groupSelector = new GroupSelector(this, sidePaneManager);
        searchManager = new SearchManager2(this, sidePaneManager);

        sidePaneManager.register("fetchMedline", medlineFetcher);
        sidePaneManager.register("CiteSeerProgress", citeSeerFetcher);
        sidePaneManager.register("CiteSeerPanel", citeSeerFetcherPanel);
        sidePaneManager.register("groups", groupSelector);
        sidePaneManager.register("search", searchManager);

        
        if (Globals.prefs.getBoolean("searchPanelVisible"))
            sidePaneManager.show("search");
    }


AboutAction aboutAction = new AboutAction();
  class AboutAction
      extends AbstractAction {
    public AboutAction() {
      super(Globals.lang("About JabRef"));

    }

    public void actionPerformed(ActionEvent e) {
      about();
    }
  }


  
  
  public void about() {
    JDialog about = new JDialog(ths, Globals.lang("About JabRef"),
                                true);
    JEditorPane jp = new JEditorPane();
    JScrollPane sp = new JScrollPane
        (jp, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
         JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    jp.setEditable(false);
    try {
      jp.setPage(GUIGlobals.class.getResource("/help/About.html"));
      
      
      jp.addHyperlinkListener(new javax.swing.event.HyperlinkListener() {
        public void hyperlinkUpdate(javax.swing.event.HyperlinkEvent e) {
          if (e.getEventType()
              == javax.swing.event.HyperlinkEvent.EventType.ACTIVATED) {
            try {
              ( (JEditorPane) e.getSource()).setPage(e.getURL());
            }
            catch (IOException ex) {}
          }
        }
      });
      about.getContentPane().add(sp);
      about.setSize(GUIGlobals.aboutSize);
      Util.placeDialog(about, ths);
      about.setVisible(true);
    }
    catch (IOException ex) {
      ex.printStackTrace();
      JOptionPane.showMessageDialog(ths, "Could not load file 'About.html'",
                                    "Error", JOptionPane.ERROR_MESSAGE);
    }

  }

  
  
  public void preferences() {
    
      AbstractWorker worker = new AbstractWorker() {
              public void run() {
                  output(Globals.lang("Opening preferences..."));
                  if (prefsDialog == null) {
                      prefsDialog = new PrefsDialog3(JabRefFrame.this);
                      Util.placeDialog(prefsDialog, JabRefFrame.this);
                  }
                  else
                      prefsDialog.setValues();

              }
              public void update() {
                  prefsDialog.setVisible(true);
                  output("");
              }
          };
      worker.getWorker().run();
      worker.getCallBack().update();
  }

public JabRefPreferences prefs() {
  return prefs;
}

  
  
  public void quit() {
    
    
    boolean close = true;
    Vector<String> filenames = new Vector<String>();
    if (tabbedPane.getTabCount() > 0) {
      for (int i = 0; i < tabbedPane.getTabCount(); i++) {
        if (baseAt(i).baseChanged) {
          tabbedPane.setSelectedIndex(i);
          int answer = JOptionPane.showConfirmDialog
              (ths, Globals.lang
               ("Database has changed. Do you "
                + "want to save before closing?"),
               Globals.lang("Save before closing"),
               JOptionPane.YES_NO_CANCEL_OPTION);

          if ( (answer == JOptionPane.CANCEL_OPTION) ||
              (answer == JOptionPane.CLOSED_OPTION)) {
            close = false; 
              return;
          }
          if (answer == JOptionPane.YES_OPTION) {
            
            try {
              basePanel().runCommand("save");
            }
            catch (Throwable ex) {
              
              
              close = false;
              break;
            }
          }
        }
        if (baseAt(i).getFile() != null) {
          filenames.add(baseAt(i).getFile().getAbsolutePath());
        }
      }
    }
    if (close) {
      dispose();

      prefs.putInt("posX", ths.getLocation().x);
      prefs.putInt("posY", ths.getLocation().y);
      prefs.putInt("sizeX", ths.getSize().width);
      prefs.putInt("sizeY", ths.getSize().height);
      prefs.putBoolean("searchPanelVisible", sidePaneManager.isComponentVisible("search"));
      
      int width = contentPane.getDividerLocation();
      if (width > 0) 
          prefs.putInt("sidePaneWidth", width);
      if (prefs.getBoolean("openLastEdited")) {
        
        
        
        if (filenames.size() == 0) {
          prefs.remove("lastEdited");
        }
        else {
          String[] names = new String[filenames.size()];
          for (int i = 0; i < filenames.size(); i++) {
            names[i] = filenames.elementAt(i);

          }
          prefs.putStringArray("lastEdited", names);
        }

      }

      fileHistory.storeHistory();
      prefs.customExports.store();
      prefs.customImports.store();
      BibtexEntryType.saveCustomEntryTypes(prefs);

      
      
      if (basePanel() != null) {
        ((SearchManager2)searchManager).updatePrefs();

      }
      System.exit(0); 
    }
  }

    

  private void macOSXRegistration() {
    if (Globals.osName.equals(Globals.MAC)) {
      try {
        Class<?> osxAdapter = Class.forName("osxadapter.OSXAdapter");

        Class<?>[] defArgs = {
            JabRefFrame.class};
        Method registerMethod = osxAdapter.getDeclaredMethod(
            "registerMacOSXApplication", defArgs);
        if (registerMethod != null) {
          Object[] args = {
              this};
          registerMethod.invoke(osxAdapter, args);
        }
        
        

        defArgs[0] = boolean.class;
        Method prefsEnableMethod = osxAdapter.getDeclaredMethod("enablePrefs",
            defArgs);
        if (prefsEnableMethod != null) {
          Object args[] = {
              Boolean.TRUE};
          prefsEnableMethod.invoke(osxAdapter, args);
        }
      }
      catch (NoClassDefFoundError e) {
        
        
        System.err.println("This version of Mac OS X does not support the Apple EAWT.  Application Menu handling has been disabled (" +
                           e + ")");
      }
      catch (ClassNotFoundException e) {
        
        
        System.err.println("This version of Mac OS X does not support the Apple EAWT.  Application Menu handling has been disabled (" +
                           e + ")");
      }
      catch (Exception e) {
        System.err.println("Exception while loading the OSXAdapter:");
        e.printStackTrace();
      }
    }
  }


  private void initLayout() {
    tabbedPane.putClientProperty(Options.NO_CONTENT_BORDER_KEY, Boolean.TRUE);

    setProgressBarVisible(false);

      pushExternalButton = new PushToApplicationButton(this,
              PushToApplicationButton.applications);
    fillMenu();
    createToolBar();
    getContentPane().setLayout(gbl);
      contentPane.setDividerSize(2);
      contentPane.setBorder(null);
    
    con.fill = GridBagConstraints.HORIZONTAL;
    con.anchor = GridBagConstraints.WEST;
    con.weightx = 1;
    con.weighty = 0;
    con.gridwidth = GridBagConstraints.REMAINDER;

    
    
    setJMenuBar(mb);
    con.anchor = GridBagConstraints.NORTH;
    
    gbl.setConstraints(tlb, con);
    getContentPane().add(tlb);

    Component lim = Box.createGlue();
    gbl.setConstraints(lim, con);
    
    
    con.gridwidth = GridBagConstraints.REMAINDER;
    con.weightx = 1;
    con.weighty = 0;
    con.fill = GridBagConstraints.BOTH;
    con.anchor = GridBagConstraints.WEST;
    con.insets = new Insets(0, 0, 0, 0);
    lim = Box.createGlue();
    gbl.setConstraints(lim, con);
    getContentPane().add(lim);
    
    
    con.weighty = 1;
    gbl.setConstraints(contentPane, con);
    getContentPane().add(contentPane);
    contentPane.setRightComponent(tabbedPane);
    contentPane.setLeftComponent(sidePaneManager.getPanel());
    sidePaneManager.updateView();

    JPanel status = new JPanel();
    status.setLayout(gbl);
    con.weighty = 0;
    con.weightx = 0;
    con.gridwidth = 1;
    con.insets = new Insets(0, 2, 0, 0);
    gbl.setConstraints(statusLabel, con);
    status.add(statusLabel);
    con.weightx = 1;
    con.insets = new Insets(0, 4, 0, 0);
    con.gridwidth = 1;
    gbl.setConstraints(statusLine, con);
    status.add(statusLine);
    con.weightx = 0;
    con.gridwidth = GridBagConstraints.REMAINDER;
    gbl.setConstraints(progressBar, con);
    status.add(progressBar);
    con.weightx = 1;
    con.gridwidth = GridBagConstraints.REMAINDER;
    statusLabel.setForeground(GUIGlobals.validFieldColor.darker());
    con.insets = new Insets(0, 0, 0, 0);
    gbl.setConstraints(status, con);
    getContentPane().add(status);


      
      TransferHandler xfer = new EntryTableTransferHandler(null, this, null);
      tabbedPane.setTransferHandler(xfer);
      tlb.setTransferHandler(xfer);
      mb.setTransferHandler(xfer);
      sidePaneManager.getPanel().setTransferHandler(xfer);
  }

  private void initLabelMaker() {
    
    labelMaker = new LabelMaker();
    labelMaker.addRule(new ArticleLabelRule(),
                       BibtexEntryType.ARTICLE);
    labelMaker.addRule(new BookLabelRule(),
                       BibtexEntryType.BOOK);
    labelMaker.addRule(new IncollectionLabelRule(),
                       BibtexEntryType.INCOLLECTION);
    labelMaker.addRule(new InproceedingsLabelRule(),
                       BibtexEntryType.INPROCEEDINGS);
  }

  
  public BasePanel baseAt(int i) {
    return (BasePanel) tabbedPane.getComponentAt(i);
  }

  public void showBaseAt(int i) {
      tabbedPane.setSelectedIndex(i);
  }

  
  public BasePanel basePanel() {
    return (BasePanel) tabbedPane.getSelectedComponent();
  }

  
  private void markActiveBasePanel()
  {
    int now = tabbedPane.getSelectedIndex() ;
    int len = tabbedPane.getTabCount() ;
    if ((lastTabbedPanelSelectionIndex > -1) && (lastTabbedPanelSelectionIndex < len))
      tabbedPane.setForegroundAt(lastTabbedPanelSelectionIndex, GUIGlobals.inActiveTabbed);
    if ( (now > -1) &&  (now < len))
      tabbedPane.setForegroundAt(now, GUIGlobals.activeTabbed);
    lastTabbedPanelSelectionIndex = now ;
  }

  private int getTabIndex(JComponent comp) {
    for (int i = 0; i < tabbedPane.getTabCount(); i++) {
      if (tabbedPane.getComponentAt(i) == comp) {
        return i;
      }
    }
    return -1;
  }

  public JTabbedPane getTabbedPane() { return tabbedPane; }

  public String getTabTitle(JComponent comp) {
    return tabbedPane.getTitleAt(getTabIndex(comp));
  }

    public String getTabTooltip(JComponent comp) {
        return tabbedPane.getToolTipTextAt(getTabIndex(comp));
    }

  public void setTabTitle(JComponent comp, String title, String toolTip) {
      int index = getTabIndex(comp);
      tabbedPane.setTitleAt(index, title);
      tabbedPane.setToolTipTextAt(index, toolTip);
  }

  class GeneralAction
      extends MnemonicAwareAction {
    private String command;
    public GeneralAction(String command, String text,
                         String description, URL icon) {
      super(new ImageIcon(icon));
      this.command = command;
      putValue(NAME, text);
      putValue(SHORT_DESCRIPTION, Globals.lang(description));
    }

    public GeneralAction(String command, String text,
                         String description, String imageName,
                         KeyStroke key) {
      super(GUIGlobals.getImage(imageName));
      this.command = command;
      putValue(NAME, text);
      putValue(ACCELERATOR_KEY, key);
      putValue(SHORT_DESCRIPTION, Globals.lang(description));
    }

      public GeneralAction(String command, String text) {
          putValue(NAME, text);
          this.command = command;
      }

      public GeneralAction(String command, String text, KeyStroke key) {
          this.command = command;
          putValue(NAME, text);
          putValue(ACCELERATOR_KEY, key);
      }

      public GeneralAction(String command, String text, String description) {
          this.command = command;
          ImageIcon icon = GUIGlobals.getImage(command);
          if (icon != null)
              putValue(SMALL_ICON, icon);
          putValue(NAME, text);
          putValue(SHORT_DESCRIPTION, Globals.lang(description));
      }

      public GeneralAction(String command, String text, String description, KeyStroke key) {
          this.command = command;
          ImageIcon icon = GUIGlobals.getImage(command);
          if (icon != null)
              putValue(SMALL_ICON, icon);
          putValue(NAME, text);
          putValue(SHORT_DESCRIPTION, Globals.lang(description));
          putValue(ACCELERATOR_KEY, key);
      }

  

    public void actionPerformed(ActionEvent e) {
      if (tabbedPane.getTabCount() > 0) {
        try {
          ( (BasePanel) (tabbedPane.getSelectedComponent ()))
              .runCommand(command);
        }
        catch (Throwable ex) {
          ex.printStackTrace();
        }
      }
      else {
        Util.pr("Action '" + command + "' must be disabled when no "
                + "database is open.");
      }
    }
  }

  

  class NewEntryAction
      extends MnemonicAwareAction {

    String type = null; 
    KeyStroke keyStroke = null; 

    public NewEntryAction(KeyStroke key) {
      
      super(GUIGlobals.getImage("add"));
      putValue(NAME, "New entry");
      putValue(ACCELERATOR_KEY, key);
      putValue(SHORT_DESCRIPTION, Globals.lang("New BibTeX entry"));
    }

    public NewEntryAction(String type_) {
      
      putValue(NAME, Util.nCase(type_));
      type = type_;
    }

    public NewEntryAction(String type_, KeyStroke key) {
        
        putValue(NAME, Util.nCase(type_));
        putValue(ACCELERATOR_KEY, key);
        type = type_;
    }

    public void actionPerformed(ActionEvent e) {
      String thisType = type;
      if (thisType == null) {
        EntryTypeDialog etd = new EntryTypeDialog(ths);
        Util.placeDialog(etd, ths);
        etd.setVisible(true);
        BibtexEntryType tp = etd.getChoice();
        if (tp == null) {
          return;
        }
        thisType = tp.getName();
      }

      if (tabbedPane.getTabCount() > 0) {
        ( (BasePanel) (tabbedPane.getSelectedComponent()))
            .newEntry(BibtexEntryType.getType(thisType));
      }
      else {
        Util.pr("Action 'New entry' must be disabled when no "
                + "database is open.");
      }
    }
  }

  

  
  public void setUpImportMenus() {
    setUpImportMenu(importMenu, false);
    setUpImportMenu(importNewMenu, true);
  }

  private void fillMenu() {
      
      mb.setBorder(null);
      JMenu file = subMenu("File"),
              sessions = subMenu("Sessions"),
              edit = subMenu("Edit"),
              bibtex = subMenu("BibTeX"),
              view = subMenu("View"),
              tools = subMenu("Tools"),
              web = subMenu("Web search"),
              options = subMenu("Options"),
              newSpec = subMenu("New entry..."),
              helpMenu = subMenu("Help");

      setUpImportMenus();

      newDatabaseMenu.add(newDatabaseAction);
      newDatabaseMenu.add(newSubDatabaseAction);

      file.add(newDatabaseAction);
      file.add(open); 
      file.add(mergeDatabaseAction);
      file.add(save);
      file.add(saveAs);
      file.add(saveAll);
      file.add(saveSelectedAs);
      file.addSeparator();
      
      
      file.add(importNew);
      file.add(importCurrent);
      file.add(exportAll);
      file.add(exportSelected);

      file.addSeparator();
      file.add(databaseProperties);
      file.addSeparator();

      sessions.add(loadSessionAction);
      sessions.add(saveSessionAction);
      file.add(sessions);
      file.add(fileHistory);
      

      file.addSeparator();
      file.add(close);
      
      
      
      
      
      
      
      
      
      
      file.add(quit);
      mb.add(file);
      edit.add(test);
      edit.add(undo);
      edit.add(redo);
      edit.addSeparator();

      edit.add(cut);
      edit.add(copy);
      edit.add(paste);
      
      edit.add(delete);
      edit.add(copyKey);
      edit.add(copyCiteKey);
      
      edit.addSeparator();
      edit.add(mark);
      edit.add(unmark);
      edit.add(unmarkAll);
      edit.addSeparator();
      edit.add(selectAll);
      mb.add(edit);
      view.add(nextTab);
      view.add(prevTab);
      view.add(sortTabs);
      view.addSeparator();
      view.add(toggleGroups);
      view.add(togglePreview);
      view.add(switchPreview);
      view.addSeparator();
      view.add(toggleHighlightAny);
      view.add(toggleHighlightAll);
      mb.add(view);

      bibtex.add(newEntryAction);
      for (int i = 0; i < newSpecificEntryAction.length; i++) {
          newSpec.add(newSpecificEntryAction[i]);
      }
      bibtex.add(newSpec);
      bibtex.add(plainTextImport);
      bibtex.addSeparator();
      bibtex.add(editEntry);
      bibtex.add(importCiteSeer);
      bibtex.add(editPreamble);
      bibtex.add(editStrings);
      mb.add(bibtex);

      tools.add(normalSearch);
      tools.add(incrementalSearch);
      tools.add(replaceAll);
      tools.add(new MassSetFieldAction(this));
      tools.add(makeKeyAction);

      
      tools.add(checkAndFix);
      checkAndFix.add(dupliCheck);
      
      checkAndFix.add(autoSetFile);
      checkAndFix.add(autoSetPdf);
      checkAndFix.add(autoSetPs);
      checkAndFix.add(integrityCheckAction);
      checkAndFix.addSeparator();
      checkAndFix.add(upgradeExternalLinks);

      tools.addSeparator();
      tools.add(manageSelectors);

      tools.add(pushExternalButton.getMenuAction());
      tools.add(writeXmpAction);

      
      
      
      
      
      tools.addSeparator();
      tools.add(openFile);
      tools.add(openUrl);
      tools.addSeparator();
      tools.add(newSubDatabaseAction);

      tools.addSeparator();
      tools.add(abbreviateIso);
      tools.add(abbreviateMedline);
      tools.add(unabbreviate);

      
      tools.addSeparator();
      tools.add(new ExpandEndnoteFilters(ths));
      
      mb.add(tools);

      web.add(fetchMedline);
      web.add(citeSeerPanelAction);
      web.add(fetchCiteSeer);
      
      
      for (EntryFetcher fetcher : fetchers){
    	  GeneralFetcher generalFetcher = new GeneralFetcher(sidePaneManager, this, fetcher);
    	  web.add(generalFetcher.getAction());
    	  fetcherActions.add(generalFetcher.getAction());
      }

      mb.add(web);

      options.add(showPrefs);
      AbstractAction customizeAction = new CustomizeEntryTypeAction();
      AbstractAction genFieldsCustomization = new GenFieldsCustomizationAction();
      options.add(customizeAction);
      options.add(genFieldsCustomization);
      options.add(customExpAction);
      options.add(customImpAction);
      options.add(customFileTypesAction);
      options.add(manageJournals);

      

      
      mb.add(options);

      helpMenu.add(help);
      helpMenu.add(contents);
      helpMenu.addSeparator();

      helpMenu.add(about);
      mb.add(helpMenu);
      helpMenu.addSeparator();
      helpMenu.add(errorConsole);
  }

    private JMenu subMenu(String name) {
        name = Globals.menuTitle(name);
        int i = name.indexOf('&');
        JMenu res;
        if (i >= 0) {
            res = new JMenu(name.substring(0, i)+name.substring(i+1));
            char mnemonic = Character.toUpperCase(name.charAt(i+1));
            res.setMnemonic((int)mnemonic);
        }
        else res = new JMenu(name);

        return res;
    }

  private void createToolBar() {
    tlb.putClientProperty(Options.HEADER_STYLE_KEY, HeaderStyle.BOTH);
    tlb.setBorder(null);
    tlb.setRollover(true);

    
    
    
    tlb.setFloatable(false);
    tlb.addAction(newDatabaseAction);
    tlb.addAction(open);
    tlb.addAction(save);
    tlb.addAction(saveAll);
    
    tlb.addSeparator();
    tlb.addAction(cut);
    tlb.addAction(copy);
    tlb.addAction(paste);
    tlb.addAction(undo);
    tlb.addAction(redo);

    tlb.addSeparator();
    tlb.addAction(newEntryAction);
    tlb.addAction(editEntry);
    tlb.addAction(editPreamble);
    tlb.addAction(editStrings);
    tlb.addAction(makeKeyAction);


    tlb.addSeparator();
    tlb.addAction(mark);
    tlb.addAction(unmark);

    tlb.addSeparator();
    searchToggle = new JToggleButton(toggleSearch);
    searchToggle.setText(null);
    if (!Globals.ON_MAC)
        searchToggle.setMargin(marg);
    tlb.add(searchToggle);

    previewToggle = new JToggleButton(togglePreview);
    previewToggle.setText(null);
    if (!Globals.ON_MAC)
        previewToggle.setMargin(marg);
    tlb.add(previewToggle);
    tlb.addSeparator();

    groupToggle = new JToggleButton(toggleGroups);
    groupToggle.setText(null);
    if (!Globals.ON_MAC)
        groupToggle.setMargin(marg);
    tlb.add(groupToggle);


    highlightAny = new JToggleButton(toggleHighlightAny);
    highlightAny.setText(null);
    if (!Globals.ON_MAC)
        highlightAny.setMargin(marg);
    tlb.add(highlightAny);
    highlightAll = new JToggleButton(toggleHighlightAll);
    highlightAll.setText(null);
    if (!Globals.ON_MAC)
        highlightAll.setMargin(marg);
    tlb.add(highlightAll);

    tlb.addSeparator();

      
      
      
      
      
      tlb.add(pushExternalButton.getComponent());

    tlb.addAction(openFile);
    tlb.addAction(openUrl);


    
    
    tlb.add(Box.createHorizontalGlue());
    

    tlb.addAction(closeDatabaseAction);
    
    
    

  }

  

 public void output(final String s) {

      SwingUtilities.invokeLater(new Runnable() {
          public void run() {
              statusLine.setText(s);
              statusLine.repaint();
          }
      });
  }

  public void stopShowingSearchResults() {
    for (int i = 0; i < tabbedPane.getTabCount(); i++) {
      baseAt(i).stopShowingSearchResults();
    }
  }

  protected List<Object> openDatabaseOnlyActions = new LinkedList<Object>();
  protected List<Object> severalDatabasesOnlyActions = new LinkedList<Object>();
  
    protected void initActions() {
        openDatabaseOnlyActions = new LinkedList<Object>();
        openDatabaseOnlyActions.addAll(Arrays.asList(new Object[] { manageSelectors,
            mergeDatabaseAction, newSubDatabaseAction, close, save, saveAs, saveSelectedAs, undo,
            redo, cut, delete, copy, paste, mark, unmark, unmarkAll, editEntry, importCiteSeer,
            selectAll, copyKey, copyCiteKey, editPreamble, editStrings, toggleGroups, toggleSearch,
            makeKeyAction, normalSearch,
            incrementalSearch, replaceAll, importMenu, exportMenu, fetchMedline, fetchCiteSeer,
            openFile, openUrl, togglePreview, dupliCheck,  highlightAll,
            highlightAny, citeSeerPanelAction, newEntryAction, plainTextImport,
            closeDatabaseAction, switchPreview, integrityCheckAction, autoSetPdf, autoSetPs,
            toggleHighlightAny, toggleHighlightAll, databaseProperties, abbreviateIso,
            abbreviateMedline, unabbreviate, exportAll, exportSelected,
            importCurrent, saveAll}));
        
        openDatabaseOnlyActions.addAll(fetcherActions);

        openDatabaseOnlyActions.addAll(Arrays.asList(newSpecificEntryAction));

        severalDatabasesOnlyActions = new LinkedList<Object>();
        severalDatabasesOnlyActions.addAll(Arrays
            .asList(new Action[] { nextTab, prevTab, sortTabs }));

        tabbedPane.addChangeListener(new ChangeListener() {
            public void stateChanged(ChangeEvent event) {
                updateEnabledState();
            }
        });
        
        

    }

    
    public static void setEnabled(List<Object> list, boolean enabled) {
        for (Object o : list){
            if (o instanceof Action)
                ((Action)o).setEnabled(enabled);
            if (o instanceof Component)
                ((Component)o).setEnabled(enabled);
        }
    }

    protected int previousTabCount = -1;
    
    
    protected void updateEnabledState() {
        int tabCount = tabbedPane.getTabCount();
        if (tabCount != previousTabCount){
            previousTabCount = tabCount;
            setEnabled(openDatabaseOnlyActions, tabCount > 0);
            setEnabled(severalDatabasesOnlyActions, tabCount > 1);
        }
    }

  
  public void setupAllTables() {
    
    
    

    
    
    for (int i = 0; i < tabbedPane.getTabCount(); i++) {
      BasePanel bf = baseAt(i);

      
      if (bf.database != null) {
        bf.setupMainPanel();

      }

    }
  }

  public BasePanel addTab(BibtexDatabase db, File file, HashMap<String, String> meta, String encoding, boolean raisePanel) {
      BasePanel bp = new BasePanel(ths, db, file, meta, encoding);
      addTab(bp, file, raisePanel);
      return bp;
  }

    public void addTab(BasePanel bp, File file, boolean raisePanel) {
        tabbedPane.add((file != null ? file.getName(): Globals.lang(GUIGlobals.untitledTitle)),
                       bp);
        tabbedPane.setToolTipTextAt(tabbedPane.getTabCount()-1,
                file != null ? file.getAbsolutePath() : null);
        if (raisePanel) {
            tabbedPane.setSelectedComponent(bp);
        }
    }

  class SelectKeysAction
      extends AbstractAction {
    public SelectKeysAction() {
      super(Globals.lang("Customize key bindings"));
    }

    public void actionPerformed(ActionEvent e) {
      KeyBindingsDialog d = new KeyBindingsDialog
          ( new HashMap<String, String>(prefs.getKeyBindings()),
           prefs.getDefaultKeys());
      d.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      d.pack(); 
      Util.placeDialog(d, ths);
      d.setVisible(true);
      if (d.getAction()) {
        prefs.setNewKeyBindings(d.getNewKeyBindings());
        JOptionPane.showMessageDialog
            (ths,
             Globals.lang("Your new key bindings have been stored.") + "\n"
             + Globals.lang("You must restart JabRef for the new key "
                            + "bindings to work properly."),
             Globals.lang("Key bindings changed"),
             JOptionPane.INFORMATION_MESSAGE);
      }
    }
  }

  
  class CloseAction
      extends MnemonicAwareAction {
    public CloseAction() {
      putValue(NAME, "Quit");
      putValue(SHORT_DESCRIPTION, Globals.lang("Quit JabRef"));
      putValue(ACCELERATOR_KEY, prefs.getKey("Quit JabRef"));
      
      

    }

    public void actionPerformed(ActionEvent e) {
      quit();
    }
  }

  
    CloseDatabaseAction closeDatabaseAction = new CloseDatabaseAction();

    class CloseDatabaseAction extends MnemonicAwareAction {
        public CloseDatabaseAction() {
            super(GUIGlobals.getImage("close"));
            putValue(NAME, "Close database");
            putValue(SHORT_DESCRIPTION, Globals.lang("Close the current database"));
            putValue(ACCELERATOR_KEY, prefs.getKey("Close database"));
        }

        public void actionPerformed(ActionEvent e) {
            
            
            boolean close = true;
            if (basePanel() == null) { 
                return; 
            }

            if (basePanel().baseChanged) {
                int answer = JOptionPane.showConfirmDialog(ths, Globals
                    .lang("Database has changed. Do you want to save " + "before closing?"),
                    Globals.lang("Save before closing"), JOptionPane.YES_NO_CANCEL_OPTION);
                if ((answer == JOptionPane.CANCEL_OPTION) || (answer == JOptionPane.CLOSED_OPTION)) {
                    close = false; 
                }
                if (answer == JOptionPane.YES_OPTION) {
                    
                    try {
                        basePanel().runCommand("save");
                    } catch (Throwable ex) {
                        
                        
                        close = false;
                    }

                }
            }

            if (close) {
                basePanel().cleanUp();
                tabbedPane.remove(basePanel());
                if (tabbedPane.getTabCount() > 0) {
                    markActiveBasePanel();
                }
                updateEnabledState(); 
                output(Globals.lang("Closed database") + ".");
                System.gc(); 
            }
        }
    }


  
  class NewDatabaseAction
      extends MnemonicAwareAction {
    public NewDatabaseAction() {
        super(GUIGlobals.getImage("new"));
        putValue(NAME, "New database");
        putValue(SHORT_DESCRIPTION, Globals.lang("New BibTeX database"));
        
    }

    public void actionPerformed(ActionEvent e) {
        
        BibtexDatabase database = new BibtexDatabase();
        addTab(database, null, null, Globals.prefs.get("defaultEncoding"), true);
        output(Globals.lang("New database created."));
    }
  }

class ImportCiteSeerAction
        extends MnemonicAwareAction {

    public ImportCiteSeerAction() {
        super(GUIGlobals.getImage("citeseer"));
        putValue(NAME, "Import Fields from CiteSeer");
        putValue(SHORT_DESCRIPTION, Globals.lang("Import Fields from CiteSeer Database"));
        putValue(ACCELERATOR_KEY, prefs.getKey("Import Fields from CiteSeer")); 
        }

        public void actionPerformed(ActionEvent e) {

                if(citeSeerFetcher.activateImportFetcher()) {


                        (new Thread() {

                                BasePanel currentBp;
                                int[] clickedOn = null;

                                class UpdateComponent implements Runnable {
                                        boolean changesMade;

                                        UpdateComponent(boolean changesMade) {
                                                this.changesMade = changesMade;
                                        }

                                        public void run() {
                                            citeSeerFetcher.endImportCiteSeerProgress();
                                            if (changesMade)
                                                    currentBp.markBaseChanged();
                                                
                                                
                                                
                                                output(Globals.lang("Completed Import Fields from CiteSeer."));
                                        }
                                }

                            public void run() {
                                currentBp = (BasePanel) tabbedPane.getSelectedComponent();
                                        

                                        int rowCount = currentBp.mainTable.getSelectedRowCount();
                                        if (rowCount >= 1) {
                                                clickedOn = currentBp.mainTable.getSelectedRows();
                                        } else {
                                                JOptionPane.showMessageDialog(currentBp.frame(),
                                                Globals.lang("You must select at least one row to perform this operation."),
                                                Globals.lang("CiteSeer Import Error"),
                                                JOptionPane.WARNING_MESSAGE);
                                        }
                                        if (clickedOn != null) {
                                                citeSeerFetcher.beginImportCiteSeerProgress();
                                                NamedCompound citeseerNamedCompound =
                                                        new NamedCompound(Globals.lang("CiteSeer Import Fields"));
                                                boolean newValues = citeSeerFetcher.importCiteSeerEntries(clickedOn, citeseerNamedCompound);
                                                if (newValues) {
                                                        citeseerNamedCompound.end();
                                                        currentBp.undoManager.addEdit(citeseerNamedCompound);
                                                }
                                                UpdateComponent updateComponent = new UpdateComponent(newValues);
                                                SwingUtilities.invokeLater(updateComponent);
                                        }
                                        citeSeerFetcher.deactivateImportFetcher();
                            }
                        }).start();
                } else {
                        JOptionPane.showMessageDialog(tabbedPane.getSelectedComponent(),
                                        Globals.lang("A CiteSeer import operation is currently in progress.") + "  " +
                                        Globals.lang("Please wait until it has finished."),
                                        Globals.lang("CiteSeer Import Error"),
                                        JOptionPane.WARNING_MESSAGE);
                }
        }
}

class FetchCiteSeerAction
        extends MnemonicAwareAction {

                public FetchCiteSeerAction() {
                    super(GUIGlobals.getImage("citeseer"));
                    putValue(NAME, "Fetch citations from CiteSeer");

                    putValue(SHORT_DESCRIPTION, Globals.lang("Fetch Articles Citing your Database"));
                    putValue(ACCELERATOR_KEY, prefs.getKey("Fetch citations from CiteSeer"));
                }

                public void actionPerformed(ActionEvent e) {

                        if(citeSeerFetcher.activateCitationFetcher()) {
                                sidePaneManager.show("CiteSeerProgress");
                                (new Thread() {
                                        BasePanel newBp;
                                        BasePanel targetBp;
                                        BibtexDatabase newDatabase;
                                        BibtexDatabase targetDatabase;

                                        Runnable updateComponent = new Runnable() {

                                                
                                                private void setSortingByCitationCount() {
                                                        newBp.sortingByCiteSeerResults = true;
                                                }

                                                public void run() {
                                                        setSortingByCitationCount();
                                                        tabbedPane.add(Globals.lang(GUIGlobals.untitledTitle), newBp);
                                                        tabbedPane.setSelectedComponent(newBp);
                                                        output(Globals.lang("Fetched all citations from target database."));
                                                        citeSeerFetcher.deactivateCitationFetcher();
                                                }
                                        };

                                  public void run() {
                                        try {
                                                newBp = new BasePanel(ths);
                                                int errorCode;
                                                targetBp = (BasePanel) tabbedPane.getSelectedComponent();
                                                newDatabase = newBp.getDatabase();
                                                targetDatabase = targetBp.getDatabase();
                                                errorCode = citeSeerFetcher.populate(newDatabase, targetDatabase);
                                                if (newDatabase.getEntryCount() > 0) {
                                                        SwingUtilities.invokeLater(updateComponent);
                                                } else if(errorCode == 0) {
                                                        SwingUtilities.invokeLater(citeSeerFetcher.getEmptyFetchSetDialog());
                                            } else {
                                                    citeSeerFetcher.deactivateCitationFetcher();
                                            }
                                        }
                                        catch (Exception ex) {
                                          ex.printStackTrace();
                                        }
                                  }
                                }).start();
                        } else {
                            JOptionPane.showMessageDialog(tabbedPane.getSelectedComponent(),
                                                Globals.lang("A CiteSeer fetch operation is currently in progress.") + "  " +
                                                Globals.lang("Please wait until it has finished."),
                                                Globals.lang("CiteSeer Fetch Error"),
                                                JOptionPane.WARNING_MESSAGE);
                        }
                }
        }



    
    class NewSubDatabaseAction extends MnemonicAwareAction
    {
      public NewSubDatabaseAction()
      {
        super(GUIGlobals.getImage("new"));
        putValue(NAME, "New subdatabase based on AUX file" );
        putValue( SHORT_DESCRIPTION, Globals.lang( "New BibTeX subdatabase" ) ) ;
            
      }

      public void actionPerformed( ActionEvent e )
      {
        

        FromAuxDialog dialog = new FromAuxDialog(ths, "", true, ths.tabbedPane) ;

        Util.placeDialog(dialog, ths);
        dialog.setVisible(true) ;

        if (dialog.okPressed())
        {
          BasePanel bp = new BasePanel( ths,
                                        dialog.getGenerateDB(),   
                                        null,                     
                                        null, Globals.prefs.get("defaultEncoding"));                     
          tabbedPane.add( Globals.lang( GUIGlobals.untitledTitle ), bp ) ;
          tabbedPane.setSelectedComponent( bp ) ;
          output( Globals.lang( "New database created." ) ) ;
        }
      }
    }


    
    class IntegrityCheckAction extends AbstractAction
    {
      public IntegrityCheckAction()
      {
        super(Globals.menuTitle("Integrity check"),
               GUIGlobals.getImage("integrityCheck")) ;
               
            
      }

      public void actionPerformed( ActionEvent e )
      {
       Object selComp = tabbedPane.getSelectedComponent() ;
       if (selComp != null)
       {
         BasePanel bp = ( BasePanel ) selComp ;
         BibtexDatabase refBase = bp.getDatabase() ;
         if (refBase != null)
         {
             IntegrityWizard wizard = new IntegrityWizard(ths, basePanel()) ;
             Util.placeDialog(wizard, ths);
             wizard.setVisible(true) ;

         }
       }
      }
    }

  class FetchMedlineAction extends MnemonicAwareAction {
        public FetchMedlineAction() {
            super(GUIGlobals.getImage("medline"));
            putValue(NAME, "Fetch Medline");
            putValue(ACCELERATOR_KEY, prefs.getKey("Fetch Medline"));
            putValue(SHORT_DESCRIPTION, Globals.lang("Fetch Medline by ID"));
        }

        public void actionPerformed(ActionEvent e) {
            if (tabbedPane.getTabCount() > 0) {
                sidePaneManager.toggle("fetchMedline");
                if (sidePaneManager.isComponentVisible("fetchMedline")) {
                    new FocusRequester(medlineFetcher.getTextField());
                }
            }
        }
    }

  class CiteSeerPanelAction extends MnemonicAwareAction {
        public CiteSeerPanelAction() {
            super(GUIGlobals.getImage("medline"));
            putValue(NAME, "Fetch CiteSeer");
            putValue(ACCELERATOR_KEY, prefs.getKey("Fetch CiteSeer"));
            putValue(SHORT_DESCRIPTION, Globals.lang("Fetch CiteSeer by ID"));
        }

        public void actionPerformed(ActionEvent e) {
            if (tabbedPane.getTabCount() > 0) {
                sidePaneManager.toggle("CiteSeerPanel");
                if (sidePaneManager.isComponentVisible("CiteSeerPanel")) {
                    new FocusRequester(citeSeerFetcherPanel.getTextField());
                }
            }
        }
    }

  
  AbstractAction showPrefs = new ShowPrefsAction();

  class ShowPrefsAction
      extends MnemonicAwareAction {
    public ShowPrefsAction() {
      super(GUIGlobals.getImage("preferences"));
      putValue(NAME, "Preferences");
      putValue(SHORT_DESCRIPTION, Globals.lang("Preferences"));
    }

    public void actionPerformed(ActionEvent e) {
      preferences();
    }
  }

  
  public void addImportedEntries(final BasePanel panel, final List<BibtexEntry> entries, String filename, boolean openInNew,
                                 ImportInspectionDialog.CallBack callBack) {
      
      
      if (Globals.prefs.getBoolean("useImportInspectionDialog") &&
              (Globals.prefs.getBoolean("useImportInspectionDialogForSingle") || (entries.size() > 1))) {
                ImportInspectionDialog diag = new ImportInspectionDialog(ths, panel,
                        BibtexFields.DEFAULT_INSPECTION_FIELDS,
                        Globals.lang("Import"), openInNew);
                diag.addEntries(entries);
                diag.addCallBack(callBack);
                diag.entryListComplete();
                Util.placeDialog(diag, ths);
                diag.setVisible(true);
        diag.toFront();
        } else {
            ths.addBibEntries(entries, filename, openInNew);
          if ((panel != null) && (entries.size() == 1)) {
              SwingUtilities.invokeLater(new Runnable() {
                  public void run() {
                      panel.highlightEntry(entries.get(0));
                  }
              });


          }
       }
  }

    
  public int addBibEntries(List<BibtexEntry> bibentries, String filename,
                           boolean intoNew) {
          if (bibentries == null || bibentries.size() == 0) {

      
      JOptionPane.showMessageDialog(ths, Globals.lang("No entries found. Please make sure you are "
                                                      +"using the correct import filter."), Globals.lang("Import failed"),
                                    JOptionPane.ERROR_MESSAGE);
      return 0;
    }

      int addedEntries = 0;

    
    Util.setAutomaticFields(bibentries);

    if (intoNew || (tabbedPane.getTabCount() == 0)) {
      
      BibtexDatabase database = new BibtexDatabase();
      for (BibtexEntry entry : bibentries){
        try {
          entry.setId(Util.createNeutralId());
          database.insertEntry(entry);
        }
        catch (KeyCollisionException ex) {
          
          System.err.println("KeyCollisionException [ addBibEntries(...) ]");
        }
      }
      HashMap<String, String> meta = new HashMap<String, String>();
      
      
      BasePanel bp = new BasePanel(ths, database, null, meta, Globals.prefs.get("defaultEncoding"));
      
      addedEntries = database.getEntryCount();
      tabbedPane.add(GUIGlobals.untitledTitle, bp);
      bp.markBaseChanged();
      tabbedPane.setSelectedComponent(bp);
      if (filename != null)
          output(Globals.lang("Imported database") + " '" + filename + "' " +
                 Globals.lang("with") + " " +
                 database.getEntryCount() + " " +
                 Globals.lang("entries into new database") + ".");
    }
    else {
      
      boolean checkForDuplicates = true;
      BasePanel basePanel = basePanel();
      BibtexDatabase database = basePanel.database;
      int oldCount = database.getEntryCount();
      NamedCompound ce = new NamedCompound(Globals.lang("Import entries"));

      mainLoop: 
      for (BibtexEntry entry : bibentries){
        boolean dupli = false;
        
        if (checkForDuplicates) {
            loop: for (Iterator<String> i2=database.getKeySet().iterator();
                       i2.hasNext();) {
                BibtexEntry existingEntry = database.getEntryById(i2.next());
                if (Util.isDuplicate(entry, existingEntry,
                                     Globals.duplicateThreshold)) {
                    DuplicateResolverDialog drd = new DuplicateResolverDialog
                        (ths, existingEntry, entry, DuplicateResolverDialog.IMPORT_CHECK);
                    drd.setVisible(true);
                    int res = drd.getSelected();
                    if (res == DuplicateResolverDialog.KEEP_LOWER)   {
                        dupli = true;
                    }
                    else if (res == DuplicateResolverDialog.KEEP_UPPER) {
                        database.removeEntry(existingEntry.getId());
                        ce.addEdit(new UndoableRemoveEntry
                                   (database, existingEntry, basePanel));
                    } else if (res == DuplicateResolverDialog.BREAK) {
                        break mainLoop;
                    }
                    break loop;
                }
            }
        }

        if (!dupli) {
            try {
                entry.setId(Util.createNeutralId());
                database.insertEntry(entry);
                ce.addEdit(new UndoableInsertEntry
                           (database, entry, basePanel));
                addedEntries++;
            }
            catch (KeyCollisionException ex) {
                
                System.err.println("KeyCollisionException [ addBibEntries(...) ]");
            }
        }
      }
        if (addedEntries > 0) {
            ce.end();
            basePanel.undoManager.addEdit(ce);
            basePanel.markBaseChanged();
            if (filename != null)
                output(Globals.lang("Imported database") + " '" + filename + "' " +
                     Globals.lang("with") + " " +
                     (database.getEntryCount() - oldCount) + " " +
                     Globals.lang("entries into new database") + ".");
        }

    }

    return addedEntries;
  }

  private void setUpImportMenu(JMenu importMenu, boolean intoNew_) {
      final boolean intoNew = intoNew_;
      importMenu.removeAll();

      
      importMenu.add(new ImportMenuItem(ths, intoNew));

      
      importMenu.addSeparator();

      SortedSet<ImportFormat> customImporters = Globals.importFormatReader.getCustomImportFormats();
      JMenu submenu = new JMenu(Globals.lang("Custom importers"));
      submenu.setMnemonic(KeyEvent.VK_S);
      
      
        for (ImportFormat imFo : customImporters){
            submenu.add(new ImportMenuItem(ths, intoNew, imFo));
        }
      
      if (customImporters.size() > 0)
          submenu.addSeparator();
      
      submenu.add(customImpAction);

      importMenu.add(submenu);
      importMenu.addSeparator();

      
      for (ImportFormat imFo : Globals.importFormatReader.getBuiltInInputFormats()){
          importMenu.add(new ImportMenuItem(ths, intoNew, imFo));
      }
  }


    public FileHistory getFileHistory() {
        return fileHistory;
    }


    
    public void setPreviewActive(boolean enabled) {
        for (int i=0; i<tabbedPane.getTabCount(); i++) {
            baseAt(i).setPreviewActive(enabled);
        }
    }


   public void removeCachedEntryEditors() {
       for (int j=0; j<tabbedPane.getTabCount(); j++) {
            BasePanel bp = (BasePanel)tabbedPane.getComponentAt(j);
            bp.entryEditors.clear();
       }
   }

    
    public void block() {
        getGlassPane().setVisible(true);
        
    }

    
    public void unblock() {
        getGlassPane().setVisible(false);
        
    }


    
    public void setProgressBarVisible(final boolean visible) {
    if (SwingUtilities.isEventDispatchThread())
        progressBar.setVisible(visible);
    else SwingUtilities.invokeLater(new Runnable() {
        public void run() {
            progressBar.setVisible(visible);
        }
        });
    }


    
    public void setProgressBarValue(final int value) {
    if (SwingUtilities.isEventDispatchThread())
        progressBar.setValue(value);
    else SwingUtilities.invokeLater(new Runnable() {
        public void run() {
            progressBar.setValue(value);
        }
        });

    }

    
    public void setProgressBarMaximum(final int value) {
    if (SwingUtilities.isEventDispatchThread())
        progressBar.setMaximum(value);
    else SwingUtilities.invokeLater(new Runnable() {
        public void run() {
            progressBar.setMaximum(value);
        }
        });


    }

class SaveSessionAction
      extends MnemonicAwareAction {
    public SaveSessionAction() {
      super(GUIGlobals.getImage("save"));
      putValue(NAME, "Save session");
      putValue(ACCELERATOR_KEY, prefs.getKey("Save session"));
    }

    public void actionPerformed(ActionEvent e) {
      
      
      
      Vector<String> filenames = new Vector<String>();
      if (tabbedPane.getTabCount() > 0) {
        for (int i = 0; i < tabbedPane.getTabCount(); i++) {
          if (tabbedPane.getTitleAt(i).equals(GUIGlobals.untitledTitle)) {
            tabbedPane.setSelectedIndex(i);
            int answer = JOptionPane.showConfirmDialog
                (ths, Globals.lang
                 ("This untitled database must be saved first to be "
                  + "included in the saved session. Save now?"),
                 Globals.lang("Save database"),
                 JOptionPane.YES_NO_OPTION);
            if (answer == JOptionPane.YES_OPTION) {
              
              try {
                basePanel().runCommand("save");
              }
              catch (Throwable ex) {}
            }
          }
          if (baseAt(i).getFile() != null) {
            filenames.add(baseAt(i).getFile().getPath());
          }
        }
      }

      if (filenames.size() == 0) {
        output(Globals.lang("Not saved (empty session)") + ".");
        return;
      }
      else {
        String[] names = new String[filenames.size()];
        for (int i = 0; i < filenames.size(); i++) {
          names[i] = filenames.elementAt(i);
        }
        prefs.putStringArray("savedSession", names);
        output(Globals.lang("Saved session") + ".");
      }

    }
  }

  class LoadSessionAction
      extends MnemonicAwareAction {
      boolean running = false;
    public LoadSessionAction() {
      super(GUIGlobals.getImage("loadSession"));
      putValue(NAME, "Load session");
      putValue(ACCELERATOR_KEY, prefs.getKey("Load session"));
    }

    public void actionPerformed(ActionEvent e) {
      if (prefs.get("savedSession") == null) {
        output(Globals.lang("No saved session found."));
        return;
      }
      if (running)
          return;
      else running = true;
      output(Globals.lang("Loading session..."));
      (new Thread() {
        public void run() {
          HashSet<String> currentFiles = new HashSet<String>();
          if (tabbedPane.getTabCount() > 0) {
            for (int i = 0; i < tabbedPane.getTabCount(); i++) {
                if (baseAt(i).getFile() != null)
                    currentFiles.add(baseAt(i).getFile().getPath());
            }
          }
          int i0 = tabbedPane.getTabCount();
          String[] names = prefs.getStringArray("savedSession");
          for (int i = 0; i < names.length; i++) {
            if (!currentFiles.contains(names[i])) {
              File file = new File(names[i]);
              if (file.exists()) {
                
                
                open.openIt(file, i == 0);
              }
            }
          }
          output(Globals.lang("Files opened") + ": " +
                 (tabbedPane.getTabCount() - i0));
          running = false;
        }
      }).start();

    }
  }

  class ChangeTabAction
      extends MnemonicAwareAction {
    private boolean next;
    public ChangeTabAction(boolean next) {
      putValue(NAME, next ? "Next tab" : "Previous tab");
      this.next = next;
      
      putValue(ACCELERATOR_KEY,
               (next ? prefs.getKey("Next tab") : prefs.getKey("Previous tab")));
    }

    public void actionPerformed(ActionEvent e) {
      int i = tabbedPane.getSelectedIndex();
      int newI = (next ? i + 1 : i - 1);
      if (newI < 0) {
        newI = tabbedPane.getTabCount() - 1;
      }
      if (newI == tabbedPane.getTabCount()) {
        newI = 0;
      }
      tabbedPane.setSelectedIndex(newI);
    }
  }

  
  class EditAction
      extends MnemonicAwareAction {
    private String command;
    public EditAction(String command, URL icon) {
      super(new ImageIcon(icon));
      this.command = command;
      String nName = Util.nCase(command);
      putValue(NAME, nName);
      putValue(ACCELERATOR_KEY, prefs.getKey(nName));
      putValue(SHORT_DESCRIPTION, Globals.lang(nName));
      
      
    }

    public void actionPerformed(ActionEvent e) {

      
      JComponent source = Globals.focusListener.getFocused();
      try {
        source.getActionMap().get(command).actionPerformed
            (new ActionEvent(source, 0, command));
      } catch (NullPointerException ex) {
        
      }
    }
  }

  class CustomizeExportsAction extends MnemonicAwareAction {
    public CustomizeExportsAction() {
      putValue(NAME, "Manage custom exports");
    }

    public void actionPerformed(ActionEvent e) {
      ExportCustomizationDialog ecd = new ExportCustomizationDialog(ths);
      ecd.setVisible(true);
    }
  }

  class CustomizeImportsAction extends MnemonicAwareAction {
    public CustomizeImportsAction() {
      putValue(NAME, "Manage custom imports");
    }

    public void actionPerformed(ActionEvent e) {
      ImportCustomizationDialog ecd = new ImportCustomizationDialog(ths);
      ecd.setVisible(true);
    }
  }

 
    class CustomizeEntryTypeAction extends MnemonicAwareAction {
        public CustomizeEntryTypeAction() {
            putValue(NAME, "Customize entry types");
        }
        public void actionPerformed(ActionEvent e) {
            JDialog dl = new EntryCustomizationDialog2(ths);
            Util.placeDialog(dl, ths);
            dl.setVisible(true);
        }
    }

    class GenFieldsCustomizationAction extends MnemonicAwareAction {
        public GenFieldsCustomizationAction() {
            putValue(NAME, "Set up general fields");
        }
        public void actionPerformed(ActionEvent e) {
            GenFieldsCustomizer gf = new GenFieldsCustomizer(ths);
            Util.placeDialog(gf, ths);
            gf.setVisible(true);

        }
    }

    class DatabasePropertiesAction extends MnemonicAwareAction {
        DatabasePropertiesDialog propertiesDialog = null;
        public DatabasePropertiesAction() {
            putValue(NAME, "Database properties");
        }

        public void actionPerformed(ActionEvent e) {
            if (propertiesDialog == null)
                propertiesDialog = new DatabasePropertiesDialog(JabRefFrame.this);
            propertiesDialog.setPanel(basePanel());
            Util.placeDialog(propertiesDialog, JabRefFrame.this);
            propertiesDialog.setVisible(true);
        }
    }

    

  private class MyGlassPane extends JPanel {
    
    public MyGlassPane() {
      addKeyListener(new KeyAdapter() { });
      addMouseListener(new MouseAdapter() { });
      
      super.setCursor(
        Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
    }
      
      public boolean isOpaque() { return false; }
  }
}
