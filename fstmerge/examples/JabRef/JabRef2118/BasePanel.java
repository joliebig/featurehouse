

package net.sf.jabref; 

import java.awt.BorderLayout; 
import java.awt.Component; 
import java.awt.GridBagConstraints; 
import java.awt.GridBagLayout; 
import java.awt.Toolkit; 
import java.awt.datatransfer.Clipboard; 
import java.awt.datatransfer.ClipboardOwner; 
import java.awt.datatransfer.DataFlavor; 
import java.awt.datatransfer.StringSelection; 
import java.awt.datatransfer.Transferable; 
import java.awt.datatransfer.UnsupportedFlavorException; 
import java.awt.event.ActionEvent; 
import java.awt.event.KeyAdapter; 
import java.awt.event.KeyEvent; 
import java.awt.event.ActionListener; 
import java.io.File; 
import java.io.IOException; 
import java.nio.charset.UnsupportedCharsetException; 
import java.util.ArrayList; 
import java.util.Arrays; 
import java.util.HashMap; 
import java.util.Iterator; 
import java.util.List; 
import java.util.Set; 
import java.util.Vector; 

import javax.swing.filechooser.FileFilter; 
import javax.swing.tree.TreePath; 
import javax.swing.undo.CannotRedoException; 
import javax.swing.undo.CannotUndoException; 

import net.sf.jabref.collab.ChangeScanner; 
import net.sf.jabref.collab.FileUpdateListener; 
import net.sf.jabref.collab.FileUpdatePanel; 
import net.sf.jabref.export.*; 
import net.sf.jabref.external.*; 
import net.sf.jabref.groups.GroupSelector; 
import net.sf.jabref.groups.GroupTreeNode; 
import net.sf.jabref.gui.*; 
import net.sf.jabref.imports.AppendDatabaseAction; 
import net.sf.jabref.imports.BibtexParser; 
import net.sf.jabref.journals.AbbreviateAction; 
import net.sf.jabref.journals.UnabbreviateAction; 
import net.sf.jabref.labelPattern.LabelPatternUtil; 
import net.sf.jabref.search.NoSearchMatcher; 
import net.sf.jabref.search.SearchMatcher; 
import net.sf.jabref.undo.CountingUndoManager; 
import net.sf.jabref.undo.NamedCompound; 
import net.sf.jabref.undo.UndoableChangeType; 
import net.sf.jabref.undo.UndoableInsertEntry; 
import net.sf.jabref.undo.UndoableKeyChange; 
import net.sf.jabref.undo.UndoableRemoveEntry; 
import net.sf.jabref.wizard.text.gui.TextInputDialog; 
import ca.odell.glazedlists.FilterList; 
import ca.odell.glazedlists.event.ListEvent; 
import ca.odell.glazedlists.event.ListEventListener; 
import ca.odell.glazedlists.matchers.Matcher; 

import com.jgoodies.forms.builder.DefaultFormBuilder; 
import com.jgoodies.forms.layout.FormLayout; 
import com.jgoodies.uif_lite.component.UIFSplitPane; 

import javax.swing.*; 

import java.awt.*; 
import java.awt.datatransfer.*; 
import java.util.*; 
import net.sf.jabref.export.ExportToClipboardAction; 
import net.sf.jabref.export.FileActions; 
import net.sf.jabref.export.SaveException; 
import net.sf.jabref.export.SaveSession; 
import net.sf.jabref.undo.*; 

public  class  BasePanel  extends JPanel implements  ClipboardOwner ,  FileUpdateListener {
	

    public final static int SHOWING_NOTHING=0, SHOWING_PREVIEW=1, SHOWING_EDITOR=2, WILL_SHOW_EDITOR=3;

	
    private int mode=0;

	
    private EntryEditor currentEditor = null;

	
    private PreviewPanel currentPreview = null;

	

    boolean tmp = true;

	

    private MainTableSelectionListener selectionListener = null;

	
    

	
    UIFSplitPane contentPane = new UIFSplitPane();

	

    JSplitPane splitPane;

	
    
    

    JabRefFrame frame;

	
    BibtexDatabase database;

	
    
    
    
    
    
    String fileMonitorHandle = null;

	
    boolean saving = false, updatedExternally = false;

	
    private String encoding;

	

    GridBagLayout gbl = new GridBagLayout();

	
    GridBagConstraints con = new GridBagConstraints();

	

    

	
    
    

    
    public CountingUndoManager undoManager = new CountingUndoManager(this);

	
    UndoAction undoAction = new UndoAction();

	
    RedoAction redoAction = new RedoAction();

	

    
    

    boolean baseChanged = false, nonUndoableChange = false;

	
    

    
    
    public MainTable mainTable = null;

	
    

	

    public RightClickMenu rcm;

	

    BibtexEntry showing = null;

	
    
    

	
    
    

    
    
    

    PreambleEditor preambleEditor = null;

	
    

    StringDialog stringDialog = null;

	
    

    SaveDatabaseAction saveAction;

	
    

    
    

    public boolean sortingBySearchResults = false,
        coloringBySearchResults = false,
    hidingNonHits = false,
        sortingByGroup = false,
        sortingByCiteSeerResults = false,
        coloringByGroup = false;

	
        
    int lastSearchHits = -1;

	 
    

    
    MetaData metaData;

	
    

	

    private boolean suppressOutput = false;

	

    private HashMap<String, Object> actions = new HashMap<String, Object>();

	
    private SidePaneManager sidePaneManager;

	

    
    public BasePanel(JabRefFrame frame) {
      this.sidePaneManager = Globals.sidePaneManager;
      database = new BibtexDatabase();
      metaData = new MetaData();
        metaData.initializeNewDatabase();
      this.frame = frame;
      setupActions();
      setupMainPanel();
        encoding = Globals.prefs.get("defaultEncoding");
        
    }


	

    
    public BasePanel(JabRefFrame frame, BibtexDatabase db, File file,
                     HashMap meta, String encoding) {

        this.encoding = encoding;
       
     
      this.sidePaneManager = Globals.sidePaneManager;
      this.frame = frame;
      database = db;
      if (meta != null)
        parseMetaData(meta);
      else {
        metaData = new MetaData();
        metaData.initializeNewDatabase();   
      }
      setupActions();
      setupMainPanel();

      metaData.setFile(file);

      
      if (file != null)
        try {
          fileMonitorHandle = Globals.fileUpdateMonitor.addUpdateListener(this,
              file);
        } catch (IOException ex) {
        }
    }


	

    public boolean isBaseChanged(){
    	return baseChanged;
    }


	
    
    public int getMode() {
        return mode;
    }


	

    public BibtexDatabase database() {
		return database;
	}


	

	public MetaData metaData() {
		return metaData;
	}


	

	public JabRefFrame frame() {
		return frame;
	}


	

	public JabRefPreferences prefs() {
		return Globals.prefs;
	}


	

	public String getEncoding() {
		return encoding;
	}


	

	public void setEncoding(String encoding) {
		this.encoding = encoding;
	}


	

    public void output(String s) {
    
        if (!suppressOutput)
            frame.output(s);
    }


	

    private void setupActions() {
        saveAction = new SaveDatabaseAction(this);
        
        actions.put("undo", undoAction);
        actions.put("redo", redoAction);

        
        actions.put("edit", new BaseAction() {
            public void action() {
                selectionListener.editSignalled();
            }
                
            });


        actions.put("test", new BaseAction () {
                public void action() throws Throwable {

                    ArrayList<BibtexEntry> entries = new ArrayList<BibtexEntry>();
                    BibtexEntry[] sel = getSelectedEntries();
                    for (int i = 0; i < sel.length; i++) {
                        BibtexEntry bibtexEntry = sel[i];
                        entries.add(bibtexEntry);
                    }
                    final List<FileListEntry> links =
                            AccessLinksForEntries.getExternalLinksForEntries(entries);
                    for (Iterator<FileListEntry> iterator = links.iterator(); iterator.hasNext();) {
                        FileListEntry entry = iterator.next();
                        System.out.println("Link: "+entry.getLink());
                    };

                    final JProgressBar prog = new JProgressBar();
                    prog.setIndeterminate(true);
                    final JDialog diag = new JDialog(frame, false);
                    diag.getContentPane().add(prog, BorderLayout.CENTER);
                    diag.pack();
                    diag.setLocationRelativeTo(frame);
                    diag.setVisible(true);
                    Thread t = new Thread(new Runnable() {
                        public void run() {
                            AccessLinksForEntries.copyExternalLinksToDirectory(links,
                                new File("/home/alver/tmp"), metaData, prog, false,
                                    new ActionListener() {
                                        public void actionPerformed(ActionEvent actionEvent) {
                                            diag.dispose();
                                        }
                                    });
                        }
                    });
                    t.start();
                    
                    
                    

                    
                    

                    
                }
            });


        
        actions.put("save", saveAction);

        actions.put("saveAs", new BaseAction() {
            public void action() throws Throwable {
                saveAction.saveAs();
            }
        });

        actions.put("saveSelectedAs", new BaseAction () {
                public void action() throws Throwable {

                  String chosenFile = Globals.getNewFile(frame, new File(Globals.prefs.get("workingDirectory")), ".bib",
                                                         JFileChooser.SAVE_DIALOG, false);
                  if (chosenFile != null) {
                    File expFile = new File(chosenFile);
                    if (!expFile.exists() ||
                        (JOptionPane.showConfirmDialog
                         (frame, "'"+expFile.getName()+"' "+
                          Globals.lang("exists. Overwrite file?"),
                          Globals.lang("Save database"), JOptionPane.OK_CANCEL_OPTION)
                         == JOptionPane.OK_OPTION)) {

                      saveDatabase(expFile, true, Globals.prefs.get("defaultEncoding"));
                      
                      frame.getFileHistory().newFile(expFile.getPath());
                      frame.output(Globals.lang("Saved selected to")+" '"
                                   +expFile.getPath()+"'.");
                        }
                    }
                }
            });
    
        
        actions.put("copy", new BaseAction() {
                public void action() {
                    BibtexEntry[] bes = mainTable.getSelectedEntries();

                    if ((bes != null) && (bes.length > 0)) {
                        TransferableBibtexEntry trbe
                            = new TransferableBibtexEntry(bes);
                        
                        Toolkit.getDefaultToolkit().getSystemClipboard()
                            .setContents(trbe, BasePanel.this);
                        output(Globals.lang("Copied")+" "+(bes.length>1 ? bes.length+" "
                                                           +Globals.lang("entries")
                                                           : "1 "+Globals.lang("entry")+"."));
                    } else {
                        
                        int[] rows = mainTable.getSelectedRows(),
                            cols = mainTable.getSelectedColumns();
                        if ((cols.length == 1) && (rows.length == 1)) {
                            
                            Object o = mainTable.getValueAt(rows[0], cols[0]);
                            if (o != null) {
                                StringSelection ss = new StringSelection(o.toString());
                                Toolkit.getDefaultToolkit().getSystemClipboard()
                                    .setContents(ss, BasePanel.this);

                                output(Globals.lang("Copied cell contents")+".");
                            }
                        }
                    }
                }
            });

        actions.put("cut", new BaseAction() {
                public void action() throws Throwable {
                    runCommand("copy");
                    BibtexEntry[] bes = mainTable.getSelectedEntries();
                    
                    if ((bes != null) && (bes.length > 0)) {
                        
                        NamedCompound ce = new NamedCompound
                        (Globals.lang(bes.length > 1 ? "cut entries" : "cut entry"));
                        
                        for (int i=0; i<bes.length; i++) {
                            database.removeEntry(bes[i].getId());
                            ensureNotShowing(bes[i]);
                            ce.addEdit(new UndoableRemoveEntry
                                       (database, bes[i], BasePanel.this));
                        }
                        
                        frame.output(Globals.lang("Cut_pr")+" "+
                                     (bes.length>1 ? bes.length
                                      +" "+ Globals.lang("entries")
                                      : Globals.lang("entry"))+".");
                        ce.end();
                        undoManager.addEdit(ce);
                        markBaseChanged();

                        
                        
                    }
                }
            });

        actions.put("delete", new BaseAction() {
                public void action() {
                  BibtexEntry[] bes = mainTable.getSelectedEntries();
                  if ((bes != null) && (bes.length > 0)) {

                      boolean goOn = showDeleteConfirmationDialog(bes.length);
                      if (!goOn) {
                          return;
                      }
                      else {
                          
                          NamedCompound ce = new NamedCompound
                              (Globals.lang(bes.length > 1 ? "delete entries" : "delete entry"));
                          
                          for (int i = 0; i < bes.length; i++) {
                              database.removeEntry(bes[i].getId());
                              ensureNotShowing(bes[i]);
                              ce.addEdit(new UndoableRemoveEntry(database, bes[i], BasePanel.this));
                          }
                          markBaseChanged();
                          frame.output(Globals.lang("Deleted") + " " +
                                       (bes.length > 1 ? bes.length
                                        + " " + Globals.lang("entries")
                                        : Globals.lang("entry")) + ".");
                          ce.end();
                          undoManager.addEdit(ce);
                          
                      }


                          
                          
                          }

                      }

            });

        
        
        
        
        
        
        
        actions.put("paste", new BaseAction() {
                public void action() {
                    
                    Transferable content = Toolkit.getDefaultToolkit()
                        .getSystemClipboard().getContents(null);
                    if (content != null) {
                        BibtexEntry[] bes = null;
                        if (content.isDataFlavorSupported(TransferableBibtexEntry.entryFlavor)) {
                            
                            try {
                                bes = (BibtexEntry[])(content.getTransferData(TransferableBibtexEntry.entryFlavor));

                            } catch (UnsupportedFlavorException ex) {
                                ex.printStackTrace();
                            } catch (IOException ex) {
                                ex.printStackTrace();
                            }
                        } else if (content.isDataFlavorSupported(DataFlavor.stringFlavor)) {
                           try {
                                  BibtexParser bp = new BibtexParser
                                      (new java.io.StringReader( (String) (content.getTransferData(
                                      DataFlavor.stringFlavor))));
                                  BibtexDatabase db = bp.parse().getDatabase();
                                  Util.pr("Parsed " + db.getEntryCount() + " entries from clipboard text");
                                  if(db.getEntryCount()>0) {
                                      bes = db.getEntries().toArray(new BibtexEntry[db.getEntryCount()]);
                                  }
                              } catch (UnsupportedFlavorException ex) {
                                  ex.printStackTrace();
                              } catch (Throwable ex) {
                                  ex.printStackTrace();
                              }

                        }

                        
                        
                        if ((bes != null) && (bes.length > 0)) {

                          NamedCompound ce = new NamedCompound
                              (Globals.lang(bes.length > 1 ? "paste entries" : "paste entry"));
                          for (int i=0; i<bes.length; i++) {
                            try {
                              BibtexEntry be = (BibtexEntry)(bes[i].clone());
                                Util.setAutomaticFields(be,
                                        Globals.prefs.getBoolean("overwriteOwner"),
                                        Globals.prefs.getBoolean("overwriteTimeStamp"));

                              
                              
                              
                              
                              
                              be.setId(Util.createNeutralId());
                              database.insertEntry(be);
                              ce.addEdit(new UndoableInsertEntry
                                         (database, be, BasePanel.this));
                            } catch (KeyCollisionException ex) {
                              Util.pr("KeyCollisionException... this shouldn't happen.");
                            }
                          }
                          ce.end();
                          undoManager.addEdit(ce);
                          
                          
                          output(Globals.lang("Pasted")+" "+
                                 (bes.length>1 ? bes.length+" "+
                                  Globals.lang("entries") : "1 "+Globals.lang("entry"))
                                 +".");
                          markBaseChanged();
                        }
                      }

                    }

});

        actions.put("selectAll", new BaseAction() {
                public void action() {
                    mainTable.selectAll();
                }
            });

        
        actions.put("editPreamble", new BaseAction() {
                public void action() {
                    if (preambleEditor == null) {
                        PreambleEditor form = new PreambleEditor
                            (frame, BasePanel.this, database, Globals.prefs);
                        Util.placeDialog(form, frame);
                        form.setVisible(true);
                        preambleEditor = form;
                    } else {
                        preambleEditor.setVisible(true);
                    }

                }
            });

        
        actions.put("editStrings", new BaseAction() {
                public void action() {
                    if (stringDialog == null) {
                        StringDialog form = new StringDialog
                            (frame, BasePanel.this, database, Globals.prefs);
                        Util.placeDialog(form, frame);
                        form.setVisible(true);
                        stringDialog = form;
                    } else {
                        stringDialog.setVisible(true);
                    }

                }
            });

        
        actions.put("toggleGroups", new BaseAction() {
            public void action() {
              sidePaneManager.toggle("groups");
              frame.groupToggle.setSelected(sidePaneManager.isComponentVisible("groups"));
            }
        });


        
        actions.put("makeKey", new AbstractWorker() {
        
        List<BibtexEntry> entries;
        int numSelected;
        boolean cancelled = false;

        
        public void init() {

                    entries = new ArrayList<BibtexEntry>(Arrays.asList(getSelectedEntries()));
                    
                    numSelected = entries.size();

                    if (entries.size() == 0) { 
                        JOptionPane.showMessageDialog(frame, Globals.lang("First select the entries you want keys to be generated for."),
                                                      Globals.lang("Autogenerate BibTeX key"), JOptionPane.INFORMATION_MESSAGE);
                        return ;
                    }
            frame.block();
            output(Globals.lang("Generating BibTeX key for")+" "+
                           numSelected+" "+(numSelected>1 ? Globals.lang("entries")
                                            : Globals.lang("entry"))+"...");
        }

        
                public void run() {
                    BibtexEntry bes = null ;
                    NamedCompound ce = new NamedCompound(Globals.lang("autogenerate keys"));

                    
                    
                    loop: for (Iterator<BibtexEntry> i=entries.iterator(); i.hasNext();) {
                        bes = i.next();
                        if (bes.getField(BibtexFields.KEY_FIELD) != null) {
                            if (Globals.prefs.getBoolean("avoidOverwritingKey"))
                                
                                i.remove();
                            else if (Globals.prefs.getBoolean("warnBeforeOverwritingKey")) {
                                
                                CheckBoxMessage cbm = new CheckBoxMessage(Globals.lang("One or more keys will be overwritten. Continue?"),
                                        Globals.lang("Disable this confirmation dialog"), false);
                                int answer = JOptionPane.showConfirmDialog(frame, cbm, Globals.lang("Overwrite keys"),
                                        JOptionPane.YES_NO_OPTION);
                                if (cbm.isSelected())
                                    Globals.prefs.putBoolean("warnBeforeOverwritingKey", false);
                                if (answer == JOptionPane.NO_OPTION) {
                                    
                                    cancelled = true;
                                    return;
                                }
                                
                                
                                break loop;
                            }
                        }
                    }

                    HashMap<BibtexEntry, Object> oldvals = new HashMap<BibtexEntry, Object>();
                    
                    
                    if (!Globals.prefs.getBoolean("avoidOverwritingKey")) for (Iterator<BibtexEntry> i=entries.iterator(); i.hasNext();) {
                        bes = i.next();
                        
                        oldvals.put(bes, bes.getField(BibtexFields.KEY_FIELD));
                        database.setCiteKeyForEntry(bes.getId(), null);
                    }

                    
                    for (Iterator<BibtexEntry> i=entries.iterator(); i.hasNext();) {
                        bes = i.next();
                        bes = LabelPatternUtil.makeLabel(Globals.prefs.getKeyPattern(), database, bes);
                        ce.addEdit(new UndoableKeyChange
                                   (database, bes.getId(), (String)oldvals.get(bes),
                                    (String)bes.getField(BibtexFields.KEY_FIELD)));
                    }
                    ce.end();
                    undoManager.addEdit(ce);
        }

        
        public void update() {
            if (cancelled) {
                frame.unblock();
                return;
            }
            markBaseChanged() ;
            numSelected = entries.size();
            output(Globals.lang("Generated BibTeX key for")+" "+
               numSelected+" "+(numSelected!=1 ? Globals.lang("entries")
                                    : Globals.lang("entry")));
            frame.unblock();
        }
    });

        actions.put("search", new BaseAction() {
                public void action() {
                    
                    sidePaneManager.show("search");
                    
                    frame.searchToggle.setSelected(true);
                    if (true)
                      frame.searchManager.startSearch();
                }
            });

        actions.put("toggleSearch", new BaseAction() {
                public void action() {
                    
                    sidePaneManager.toggle("search");
                    boolean on = sidePaneManager.isComponentVisible("search");
                    frame.searchToggle.setSelected(on);
                    if (on)
                      frame.searchManager.startSearch();
                }
            });

        actions.put("incSearch", new BaseAction() {
                public void action() {
                    sidePaneManager.show("search");
                    frame.searchToggle.setSelected(true);
                    frame.searchManager.startIncrementalSearch();
                }
            });

        
        actions.put("copyKey", new BaseAction() {
                public void action() {
                    BibtexEntry[] bes = mainTable.getSelectedEntries();
                    if ((bes != null) && (bes.length > 0)) {
                        storeCurrentEdit();
                        
                        Vector<Object> keys = new Vector<Object>();
                        
                        for (int i=0; i<bes.length; i++)
                            if (bes[i].getField(BibtexFields.KEY_FIELD) != null)
                                keys.add(bes[i].getField(BibtexFields.KEY_FIELD));
                        if (keys.size() == 0) {
                            output("None of the selected entries have BibTeX keys.");
                            return;
                        }
                        StringBuffer sb = new StringBuffer((String)keys.elementAt(0));
                        for (int i=1; i<keys.size(); i++) {
                            sb.append(',');
                            sb.append((String)keys.elementAt(i));
                        }

                        StringSelection ss = new StringSelection(sb.toString());
                        Toolkit.getDefaultToolkit().getSystemClipboard()
                            .setContents(ss, BasePanel.this);

                        if (keys.size() == bes.length)
                            
                            output(Globals.lang((bes.length > 1) ? "Copied keys"
                                                : "Copied key")+".");
                        else
                            output(Globals.lang("Warning")+": "+(bes.length-keys.size())
                                   +" "+Globals.lang("out of")+" "+bes.length+" "+
                                   Globals.lang("entries have undefined BibTeX key")+".");
                    }
                }
            });

        
        actions.put("copyCiteKey", new BaseAction() {
                public void action() {
                    BibtexEntry[] bes = mainTable.getSelectedEntries();
                    if ((bes != null) && (bes.length > 0)) {
                        storeCurrentEdit();
                        
                        Vector<Object> keys = new Vector<Object>();
                        
                        for (int i=0; i<bes.length; i++)
                            if (bes[i].getField(BibtexFields.KEY_FIELD) != null)
                                keys.add(bes[i].getField(BibtexFields.KEY_FIELD));
                        if (keys.size() == 0) {
                            output("None of the selected entries have BibTeX keys.");
                            return;
                        }
                        StringBuffer sb = new StringBuffer((String)keys.elementAt(0));
                        for (int i=1; i<keys.size(); i++) {
                            sb.append(',');
                            sb.append((String)keys.elementAt(i));
                        }

                        StringSelection ss = new StringSelection
                            ("\\cite{"+sb.toString()+"}");
                        Toolkit.getDefaultToolkit().getSystemClipboard()
                            .setContents(ss, BasePanel.this);

                        if (keys.size() == bes.length)
                            
                            output(Globals.lang((bes.length > 1) ? "Copied keys"
                                                : "Copied key")+".");
                        else
                            output(Globals.lang("Warning")+": "+(bes.length-keys.size())
                                   +" "+Globals.lang("out of")+" "+bes.length+" "+
                                   Globals.lang("entries have undefined BibTeX key")+".");
                    }
                }
            });

          actions.put("mergeDatabase", new AppendDatabaseAction(frame, this));


        actions.put("openFile", new BaseAction() {
            public void action() {
                (new Thread() {
                    public void run() {
                        BibtexEntry[] bes = mainTable.getSelectedEntries();
                        String field = "ps";
                        if ((bes != null) && (bes.length == 1)) {
                            Object link = bes[0].getField("ps");
                            if (bes[0].getField("pdf") != null) {
                                link = bes[0].getField("pdf");
                                field = "pdf";
                            }
                            String filepath = null;
                            if (link != null) {
                                filepath = link.toString();
                            } else {

                                
                                String basefile;
                                Object key = bes[0].getField(BibtexFields.KEY_FIELD);
                                if (key != null) {
                                    basefile = key.toString();
                                    final String[] types = new String[]{"pdf", "ps"};
                                    final String sep = System.getProperty("file.separator");
                                    for (int i = 0; i < types.length; i++) {
                                        String dir = Globals.prefs.get(types[i] + "Directory");
                                        if (dir != null) {
                                            if (dir.endsWith(sep)) {
                                                dir = dir.substring(0, dir.length() - sep.length());
                                            }
                                        } else
                                            dir = "";
                                        String found = Util.findPdf(basefile, types[i], dir, new OpenFileFilter("." + types[i]));
                                        if (found != null) {
                                            filepath = dir + sep + found;
                                            break;
                                        }
                                    }
                                }
                            }


                            if (filepath != null) {
                                
                                try {
                                    Util.openExternalViewer(metaData(), filepath, field);
                                    output(Globals.lang("External viewer called") + ".");
                                }
                                catch (IOException ex) {
                                    output(Globals.lang("Error") + ": " + ex.getMessage());
                                }
                            } else
                                output(Globals.lang(
                                        "No pdf or ps defined, and no file matching Bibtex key found") +
                                        ".");
                        } else
                            output(Globals.lang("No entries or multiple entries selected."));
                    }
                }).start();
            }
        });

        actions.put("openExternalFile", new BaseAction() {
            public void action() {
                (new Thread() {
                    public void run() {
                        BibtexEntry[] bes = mainTable.getSelectedEntries();
                        String field = GUIGlobals.FILE_FIELD;
                        if ((bes != null) && (bes.length == 1)) {
                            Object link = bes[0].getField(field);
                            if (link == null) {
                                runCommand("openFile"); 
                                return;
                            }
                            FileListTableModel tableModel = new FileListTableModel();
                            tableModel.setContent((String)link);
                            if (tableModel.getRowCount() == 0) {
                                runCommand("openFile"); 
                                return;
                            }
                            FileListEntry flEntry = tableModel.getEntry(0);
                            ExternalFileMenuItem item = new ExternalFileMenuItem
                                (frame(), bes[0], "",
                                flEntry.getLink(), flEntry.getType().getIcon(),
                                metaData(), flEntry.getType());
                            item.actionPerformed(null);
                        } else
                            output(Globals.lang("No entries or multiple entries selected."));
                    }
                }).start();
            }
        });


        actions.put("openUrl", new BaseAction() {
                      public void action() {
                          BibtexEntry[] bes = mainTable.getSelectedEntries();
                          String field = "doi";
                          if ((bes != null) && (bes.length == 1)) {
                              Object link = bes[0].getField("doi");
                              if (bes[0].getField("url") != null) {
                                link = bes[0].getField("url");
                                field = "url";
                              }
                              if (link != null) {
                                
                                try {
                                  Util.openExternalViewer(metaData(), link.toString(), field);
                                  output(Globals.lang("External viewer called")+".");
                                } catch (IOException ex) {
                                    output(Globals.lang("Error") + ": " + ex.getMessage());
                                }
                              }
                              else
                                  output(Globals.lang("No url defined")+".");
                          } else
                            output(Globals.lang("No entries or multiple entries selected."));
                      }
              });

          actions.put("replaceAll", new BaseAction() {
                    public void action() {
                      ReplaceStringDialog rsd = new ReplaceStringDialog(frame);
                      rsd.setVisible(true);
                      if (!rsd.okPressed())
                          return;
                      int counter = 0;
                      NamedCompound ce = new NamedCompound(Globals.lang("Replace string"));
                      if (!rsd.selOnly()) {
                    	  for (BibtexEntry entry : database.getEntries()){
                              counter += rsd.replace(entry, ce);
                    	  }
                      } else {
                          BibtexEntry[] bes = mainTable.getSelectedEntries();
                          for (int i=0; i<bes.length; i++)
                              counter += rsd.replace(bes[i], ce);
                      }

                      output(Globals.lang("Replaced")+" "+counter+" "+
                             Globals.lang(counter==1?"occurence":"occurences")+".");
                      if (counter > 0) {
                          ce.end();
                          undoManager.addEdit(ce);
                          markBaseChanged();
                      }
                  }
              });

              actions.put("dupliCheck", new BaseAction() {
                public void action() {
                  DuplicateSearch ds = new DuplicateSearch(BasePanel.this);
                  ds.start();
                }
              });

              

              actions.put("plainTextImport", new BaseAction() {
                public void action()
                {
                  
                  EntryTypeDialog etd = new EntryTypeDialog(frame);
                  Util.placeDialog(etd, BasePanel.this);
                  etd.setVisible(true);
                  BibtexEntryType tp = etd.getChoice();
                  if (tp == null)
                    return;

                  String id = Util.createNeutralId();
                  BibtexEntry bibEntry = new BibtexEntry(id, tp) ;
                  TextInputDialog tidialog = new TextInputDialog(frame, BasePanel.this,
                                                                 "import", true,
                                                                 bibEntry) ;
                  Util.placeDialog(tidialog, BasePanel.this);
                  tidialog.setVisible(true);

                  if (tidialog.okPressed())
                  {
                      Util.setAutomaticFields(Arrays.asList(new BibtexEntry[] {bibEntry}),
                              false, false);
                    insertEntry(bibEntry) ;
                  }
                }
              });

              
              
              actions.put("markEntries", new AbstractWorker() {
                  private int besLength = -1;
                public void run() {

                  NamedCompound ce = new NamedCompound(Globals.lang("Mark entries"));
                  BibtexEntry[] bes = mainTable.getSelectedEntries();
                  besLength = bes.length;
          if (bes == null)
              return;
                  for (int i=0; i<bes.length; i++) {
                      Util.markEntry(bes[i], ce);
                  }
                  ce.end();
                  undoManager.addEdit(ce);
                }

                public void update() {
                  markBaseChanged();
                  output(Globals.lang("Marked selected")+" "+Globals.lang(besLength>0?"entry":"entries"));

                }
              });

              actions.put("unmarkEntries", new BaseAction() {
                public void action() {
                    try {
                  NamedCompound ce = new NamedCompound(Globals.lang("Unmark entries"));
                  BibtexEntry[] bes = mainTable.getSelectedEntries();
          if (bes == null)
              return;
                  for (int i=0; i<bes.length; i++) {
                      Util.unmarkEntry(bes[i], database, ce);
                  }
                  ce.end();
                  undoManager.addEdit(ce);
                  markBaseChanged();
                  output(Globals.lang("Unmarked selected")+" "+Globals.lang(bes.length>0?"entry":"entries"));
                    } catch (Throwable ex) { ex.printStackTrace(); }
                }
              });

              actions.put("unmarkAll", new BaseAction() {
                public void action() {
                  NamedCompound ce = new NamedCompound(Globals.lang("Unmark all"));
                  
                  for (BibtexEntry be : database.getEntries()){
                    Util.unmarkEntry(be, database, ce);
                  }
                  ce.end();
                  undoManager.addEdit(ce);
                  markBaseChanged();
                }
              });

              actions.put("togglePreview", new BaseAction() {
                      public void action() {
                          boolean enabled = !Globals.prefs.getBoolean("previewEnabled");
                          Globals.prefs.putBoolean("previewEnabled", enabled);
                          frame.setPreviewActive(enabled);
                          frame.previewToggle.setSelected(enabled);
                      }
                  });

              actions.put("toggleHighlightGroupsMatchingAny", new BaseAction() {
                public void action() {
                    boolean enabled = !Globals.prefs.getBoolean("highlightGroupsMatchingAny");
                    Globals.prefs.putBoolean("highlightGroupsMatchingAny", enabled);
                    frame.highlightAny.setSelected(enabled);
                    if (enabled) {
                        frame.highlightAll.setSelected(false);
                        Globals.prefs.putBoolean("highlightGroupsMatchingAll", false);
                    }
                    
                    groupsHighlightListener.listChanged(null);
                }
              });

              actions.put("toggleHighlightGroupsMatchingAll", new BaseAction() {
                  public void action() {
                      boolean enabled = !Globals.prefs.getBoolean("highlightGroupsMatchingAll");
                      Globals.prefs.putBoolean("highlightGroupsMatchingAll", enabled);
                      frame.highlightAll.setSelected(enabled);
                      if (enabled) {
                          frame.highlightAny.setSelected(false);
                          Globals.prefs.putBoolean("highlightGroupsMatchingAny", false);
                      }
                      
                      groupsHighlightListener.listChanged(null);
                  }
                });

              actions.put("switchPreview", new BaseAction() {
                      public void action() {
                          selectionListener.switchPreview();
                      }
                  });

              actions.put("manageSelectors", new BaseAction() {
                      public void action() {
                          ContentSelectorDialog2 csd = new ContentSelectorDialog2
                              (frame, frame, BasePanel.this, false, metaData, null);
                          Util.placeDialog(csd, frame);
                          csd.setVisible(true);
                      }
                  });


          actions.put("exportToClipboard", new ExportToClipboardAction(frame, database()));
        
        actions.put("writeXMP", new WriteXMPAction(this));
        
        actions.put("abbreviateIso", new AbbreviateAction(this, true));
        actions.put("abbreviateMedline", new AbbreviateAction(this, false));
        actions.put("unabbreviate", new UnabbreviateAction(this));
        actions.put("autoSetPdf", new AutoSetExternalFileForEntries(this, "pdf"));
        actions.put("autoSetPs", new AutoSetExternalFileForEntries(this, "ps"));
        actions.put("autoSetFile", new SynchronizeFileField(this));
        actions.put("upgradeLinks", new UpgradeExternalLinks(this));

    }


	

    
    public void runCommand(String _command) {
      final String command = _command;
      
      
          if (actions.get(command) == null)
            Util.pr("No action defined for'" + command + "'");
            else {
        Object o = actions.get(command);
        try {
            if (o instanceof BaseAction)
            ((BaseAction)o).action();
            else {
            
            Worker wrk = ((AbstractWorker)o).getWorker();
            
            
            
            CallBack clb = ((AbstractWorker)o).getCallBack();

            ((AbstractWorker)o).init(); 
            

            
            
            
            wrk.run(); 
            
            
            clb.update(); 
            }
        } catch (Throwable ex) {
            
            
            
            frame.unblock();
            ex.printStackTrace();
        }
        }
      
      
    }


	

    private boolean saveDatabase(File file, boolean selectedOnly, String encoding) throws SaveException {
        SaveSession session;
        frame.block();
        try {
            if (!selectedOnly)
                session = FileActions.saveDatabase(database, metaData, file,
                                           Globals.prefs, false, false, encoding);
            else
                session = FileActions.savePartOfDatabase(database, metaData, file,
                                               Globals.prefs, mainTable.getSelectedEntries(), encoding);

        } catch (UnsupportedCharsetException ex2) {
            JOptionPane.showMessageDialog(frame, Globals.lang("Could not save file. "
                +"Character encoding '%0' is not supported.", encoding),
                    Globals.lang("Save database"), JOptionPane.ERROR_MESSAGE);
            throw new SaveException("rt");
        } catch (SaveException ex) {
            if (ex.specificEntry()) {
                
                
                int row = mainTable.findEntry(ex.getEntry()),
                    topShow = Math.max(0, row-3);
                mainTable.setRowSelectionInterval(row, row);
                mainTable.scrollTo(topShow);
                showEntry(ex.getEntry());
            }
            else ex.printStackTrace();

            JOptionPane.showMessageDialog
                (frame, Globals.lang("Could not save file")
                 +".\n"+ex.getMessage(),
                 Globals.lang("Save database"),
                 JOptionPane.ERROR_MESSAGE);
            throw new SaveException("rt");

        } finally {
            frame.unblock();
        }

        boolean commit = true;
        if (!session.getWriter().couldEncodeAll()) {
            DefaultFormBuilder builder = new DefaultFormBuilder(new FormLayout("left:pref, 4dlu, fill:pref", ""));
            JTextArea ta = new JTextArea(session.getWriter().getProblemCharacters());
            ta.setEditable(false);
            builder.append(Globals.lang("The chosen encoding '%0' could not encode the following characters: ",
                      session.getEncoding()));
            builder.append(ta);
            builder.append(Globals.lang("What do you want to do?"));
            String tryDiff = Globals.lang("Try different encoding");
            int answer = JOptionPane.showOptionDialog(frame, builder.getPanel(), Globals.lang("Save database"),
                    JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.WARNING_MESSAGE, null,
                    new String[] {Globals.lang("Save"), tryDiff, Globals.lang("Cancel")}, tryDiff);

            if (answer == JOptionPane.NO_OPTION) {
                
                Object choice = JOptionPane.showInputDialog(frame, Globals.lang("Select encoding"), Globals.lang("Save database"),
                        JOptionPane.QUESTION_MESSAGE, null, Globals.ENCODINGS, encoding);
                if (choice != null) {
                    String newEncoding = (String)choice;
                    return saveDatabase(file, selectedOnly, newEncoding);
                } else
                    commit = false;
            } else if (answer == JOptionPane.CANCEL_OPTION)
                    commit = false;


          }

        try {
            if (commit) {
                session.commit();
                this.encoding = encoding; 
            }
            else
                session.cancel();
        } catch (IOException e) {
            e.printStackTrace();
        }

        return commit;
    }


	


    
    public BibtexEntry newEntry(BibtexEntryType type) {
        if (type == null) {
            
            EntryTypeDialog etd = new EntryTypeDialog(frame);
            
            Util.placeDialog(etd, frame);
            etd.setVisible(true);
            type = etd.getChoice();
        }
        if (type != null) { 
            String id = Util.createNeutralId();
            final BibtexEntry be = new BibtexEntry(id, type);
            try {
                database.insertEntry(be);

                
                ArrayList<BibtexEntry> list = new ArrayList<BibtexEntry>();
                list.add(be);
                Util.setAutomaticFields(list, true, true);

                
                undoManager.addEdit(new UndoableInsertEntry(database, be, BasePanel.this));
                output(Globals.lang("Added new")+" '"+type.getName().toLowerCase()+"' "
                       +Globals.lang("entry")+".");
                mainTable.findEntry(be);

                
                
                
                
                if (mode != SHOWING_EDITOR) {
                    mode = WILL_SHOW_EDITOR;
                }

                highlightEntry(be);  

                markBaseChanged(); 
                new FocusRequester(getEntryEditor(be));
                return be;
            } catch (KeyCollisionException ex) {
                Util.pr(ex.getMessage());
            }
        }
        return null;
    }


	



    
    public void insertEntry(BibtexEntry bibEntry)
    {
      if (bibEntry != null)
      {
        try
        {
          database.insertEntry(bibEntry) ;
          if (Globals.prefs.getBoolean("useOwner"))
            
            bibEntry.setField(BibtexFields.OWNER, Globals.prefs.get("defaultOwner") );
            
            undoManager.addEdit(new UndoableInsertEntry(database, bibEntry, BasePanel.this));
            output(Globals.lang("Added new")+" '"
                   +bibEntry.getType().getName().toLowerCase()+"' "
                   +Globals.lang("entry")+".");
            int row = mainTable.findEntry(bibEntry);

            mainTable.clearSelection();
            mainTable.scrollTo(row);
            markBaseChanged(); 
            if (Globals.prefs.getBoolean("autoOpenForm"))
            {
                  showEntry(bibEntry);
            }
        } catch (KeyCollisionException ex) { Util.pr(ex.getMessage()); }
      }
    }


	

    public void createMainTable() {
        

        GlazedEntrySorter eventList = new GlazedEntrySorter(database.getEntryMap());
        

        database.addDatabaseChangeListener(eventList);
        groupFilterList = new FilterList<BibtexEntry>(eventList.getTheList(), NoSearchMatcher.INSTANCE);
        searchFilterList = new FilterList<BibtexEntry>(groupFilterList, NoSearchMatcher.INSTANCE);
        
        MainTableFormat tableFormat = new MainTableFormat(this);
        tableFormat.updateTableFormat();
        
        mainTable = new MainTable(tableFormat, searchFilterList, frame, this);
        
        selectionListener = new MainTableSelectionListener(this, mainTable);
        mainTable.updateFont();
        mainTable.addSelectionListener(selectionListener);
        mainTable.addMouseListener(selectionListener);
        mainTable.addKeyListener(selectionListener);
        mainTable.addFocusListener(selectionListener);
        
        
        groupsHighlightListener = new ListEventListener<BibtexEntry>() {
            public void listChanged(ListEvent<BibtexEntry> listEvent) {
                if (Globals.prefs.getBoolean("highlightGroupsMatchingAny"))
                    getGroupSelector().showMatchingGroups(
                            mainTable.getSelectedEntries(), false);
                else if (Globals.prefs.getBoolean("highlightGroupsMatchingAll"))
                    getGroupSelector().showMatchingGroups(
                            mainTable.getSelectedEntries(), true);
                else 
                    getGroupSelector().showMatchingGroups(null, true);
            }
        };
        mainTable.addSelectionListener(groupsHighlightListener);

        mainTable.getActionMap().put("cut", new AbstractAction() {
                public void actionPerformed(ActionEvent e) {
                    try { runCommand("cut");
                    } catch (Throwable ex) {
                        ex.printStackTrace();
                    }
                }
            });
        mainTable.getActionMap().put("copy", new AbstractAction() {
                public void actionPerformed(ActionEvent e) {
                    try { runCommand("copy");
                    } catch (Throwable ex) {
                        ex.printStackTrace();
                    }
                }
            });
        mainTable.getActionMap().put("paste", new AbstractAction() {
                public void actionPerformed(ActionEvent e) {
                    try { runCommand("paste");
                    } catch (Throwable ex) {
                        ex.printStackTrace();
                    }
                }
            });

        mainTable.addKeyListener(new KeyAdapter() {

                public void keyPressed(KeyEvent e) {
                    final int keyCode = e.getKeyCode();
                    final TreePath path = frame.groupSelector.getSelectionPath();
                    final GroupTreeNode node = path == null ? null : (GroupTreeNode) path.getLastPathComponent();

                    if (e.isControlDown()) {
                        switch (keyCode) {
                        
                        
                        
                        case KeyEvent.VK_UP:
                            e.consume();
                            if (node != null)
                                frame.groupSelector.moveNodeUp(node, true);
                            break;
                        case KeyEvent.VK_DOWN:
                            e.consume();
                            if (node != null)
                                frame.groupSelector.moveNodeDown(node, true);
                            break;
                        case KeyEvent.VK_LEFT:
                            e.consume();
                            if (node != null)
                                frame.groupSelector.moveNodeLeft(node, true);
                            break;
                        case KeyEvent.VK_RIGHT:
                            e.consume();
                            if (node != null)
                                frame.groupSelector.moveNodeRight(node, true);
                            break;
                        case KeyEvent.VK_PAGE_DOWN:
                            frame.nextTab.actionPerformed(null);
                            e.consume();
                            break;
                        case KeyEvent.VK_PAGE_UP:
                            frame.prevTab.actionPerformed(null);
                            e.consume();
                            break;
                        }
                    } else if (keyCode == KeyEvent.VK_ENTER){
                        e.consume();
                        try { runCommand("edit");
                        } catch (Throwable ex) {
                            ex.printStackTrace();
                        }
                    }
                }
        });
    }


	

    public void setupMainPanel() {
        
        
        splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        splitPane.setDividerSize(GUIGlobals.SPLIT_PANE_DIVIDER_SIZE);
        
        
        
        

        createMainTable();

        splitPane.setTopComponent(mainTable.getPane());

        
        
        
        if (mode == SHOWING_PREVIEW) {
            mode = SHOWING_NOTHING;
            int row = mainTable.findEntry(currentPreview.entry);
            if (row >= 0)
                mainTable.setRowSelectionInterval(row, row);

        }
        else if (mode == SHOWING_EDITOR) {
            mode = SHOWING_NOTHING;
            
            
        } else
            splitPane.setBottomComponent(null);


        setLayout(new BorderLayout());
        removeAll();
        add(splitPane, BorderLayout.CENTER);

        
        if (Globals.prefs.getBoolean("autoComplete")) {
            instantiateAutoCompleters();
        }

        splitPane.revalidate();
        revalidate();
        repaint();
    }


	

    public HashMap<String, AutoCompleter> getAutoCompleters() {
        return autoCompleters;
    }


	
    
    public AutoCompleter getAutoCompleter(String fieldName) {
        return autoCompleters.get(fieldName);
    }


	

    private void instantiateAutoCompleters() {
        autoCompleters.clear();
        String[] completeFields = Globals.prefs.getStringArray("autoCompleteFields");
        for (int i = 0; i < completeFields.length; i++) {
            String field = completeFields[i];
            autoCompleters.put(field, new AutoCompleter(field));
        }
        for (BibtexEntry entry : database.getEntries()){
            Util.updateCompletersForEntry(autoCompleters, entry);
        }
    }


	


    
    


	

    

    public void updatePreamble() {
        if (preambleEditor != null)
            preambleEditor.updatePreamble();
    }


	

    public void assureStringDialogNotEditing() {
        if (stringDialog != null)
            stringDialog.assureNotEditing();
    }


	

    public void updateStringDialog() {
        if (stringDialog != null)
            stringDialog.refreshTable();
    }


	

    public void updateEntryPreviewToRow(BibtexEntry e) {

    }


	

    public void adjustSplitter() {
        int mode = getMode();
        if (mode == SHOWING_PREVIEW) {
            splitPane.setDividerLocation(splitPane.getHeight()-GUIGlobals.PREVIEW_PANEL_HEIGHT);
        } else {
            splitPane.setDividerLocation(GUIGlobals.VERTICAL_DIVIDER_LOCATION);

        }
    }


	



    
    public boolean entryEditorAllowsChange() {
      Component c = splitPane.getBottomComponent();
      if ((c != null) && (c instanceof EntryEditor)) {
        return ((EntryEditor)c).lastSourceAccepted();
      }
      else
        return true;
    }


	

    public void moveFocusToEntryEditor() {
      Component c = splitPane.getBottomComponent();
      if ((c != null) && (c instanceof EntryEditor)) {
        new FocusRequester(c);
      }
    }


	

    
    public void hidePreview() {
        Globals.prefs.putBoolean("previewEnabled", false);

      Component c = splitPane.getBottomComponent();
      if ((c != null) && !(c instanceof EntryEditor))
        splitPane.setBottomComponent(null);
    }


	

    public boolean isShowingEditor() {
      return ((splitPane.getBottomComponent() != null)
              && (splitPane.getBottomComponent() instanceof EntryEditor));
    }


	

    public void showEntry(final BibtexEntry be) {
        if (showing == be) {
            if (splitPane.getBottomComponent() == null) {
                
                
                
                
                
                showing = null;
                showEntry(be);
            } else {
              
              
              ((EntryEditor)splitPane.getBottomComponent()).updateAllFields();

            }
            return;

        }

        EntryEditor form;
        int divLoc = -1;
        String visName = null;
        if (showing != null) {
            visName = ((EntryEditor)splitPane.getBottomComponent()).
                getVisiblePanelName();
        }
        if (showing != null)
            divLoc = splitPane.getDividerLocation();

        if (entryEditors.containsKey(be.getType().getName())) {
            
            form = entryEditors.get
                ((be.getType().getName()));
            form.switchTo(be);
            if (visName != null)
                form.setVisiblePanel(visName);
            splitPane.setBottomComponent(form);
            
        } else {
            
            form = new EntryEditor(frame, BasePanel.this, be);
            if (visName != null)
                form.setVisiblePanel(visName);
            splitPane.setBottomComponent(form);

            
            entryEditors.put(be.getType().getName(), form);

        }
        if (divLoc > 0) {
          splitPane.setDividerLocation(divLoc);
        }
        else
            splitPane.setDividerLocation
                (GUIGlobals.VERTICAL_DIVIDER_LOCATION);
        
        

        showing = be;
        setEntryEditorEnabled(true); 
    }


	

    
    public EntryEditor getEntryEditor(BibtexEntry entry) {
        EntryEditor form;
        if (entryEditors.containsKey(entry.getType().getName())) {
            
            form = entryEditors.get
                ((entry.getType().getName()));

            form.switchTo(entry);
            
            
        } else {
            
            form = new EntryEditor(frame, BasePanel.this, entry);
            
            

            entryEditors.put(entry.getType().getName(), form);
        }
        return form;
    }


	

    public EntryEditor getCurrentEditor() {
        return currentEditor;
    }


	

    
    public void showEntryEditor(EntryEditor editor) {
        int oldSplitterLocation = -1;
        if (mode == SHOWING_EDITOR)
            oldSplitterLocation = splitPane.getDividerLocation();
        boolean adjustSplitter = (mode == WILL_SHOW_EDITOR);
        mode = SHOWING_EDITOR;
        currentEditor = editor;
        splitPane.setBottomComponent(editor);
        if (oldSplitterLocation > 0)
            splitPane.setDividerLocation(oldSplitterLocation);
        if (adjustSplitter) {
            adjustSplitter();
            
        }
    }


	

    
    public void showPreview(PreviewPanel preview) {
        mode = SHOWING_PREVIEW;
        currentPreview = preview;
        splitPane.setBottomComponent(preview.getPane());
    }


	

    
    public void hideBottomComponent() {
        mode = SHOWING_NOTHING;
        splitPane.setBottomComponent(null);
    }


	

    
    public void highlightEntry(final BibtexEntry be) {
        
        
                 final int row = mainTable.findEntry(be);
                 if (row >= 0) {
                    mainTable.setRowSelectionInterval(row, row);
                    
                    mainTable.ensureVisible(row);
                 }
        
        
    }


	


    
    public void entryEditorClosing(EntryEditor editor) {
        selectionListener.entryEditorClosing(editor);
    }


	

    
    

    
    public void ensureNotShowing(BibtexEntry be) {
        if ((mode == SHOWING_EDITOR) && (currentEditor.getEntry() == be)) {
            selectionListener.entryEditorClosing(currentEditor);
        }
    }


	

    public void updateEntryEditorIfShowing() {
        if (mode == SHOWING_EDITOR) {
            if (currentEditor.getType() != currentEditor.getEntry().getType()) {
                
                showing = null;
                EntryEditor newEditor = getEntryEditor(currentEditor.getEntry());
                showEntryEditor(newEditor);
            } else {
                currentEditor.updateAllFields();
                currentEditor.updateSource();
            }
        }
    }


	

    
    public void storeCurrentEdit() {
        if (isShowingEditor()) {
            EntryEditor editor = (EntryEditor)splitPane.getBottomComponent();
            editor.storeCurrentEdit();
        }

    }


	

    
    public void updateAllContentSelectors() {
        for (Iterator<String> i=entryEditors.keySet().iterator(); i.hasNext();) {
            EntryEditor ed = entryEditors.get(i.next());
            ed.updateAllContentSelectors();
        }
    }


	

    public void rebuildAllEntryEditors() {
        for (Iterator<String> i=entryEditors.keySet().iterator(); i.hasNext();) {
            EntryEditor ed = entryEditors.get(i.next());
            ed.rebuildPanels();
        }

    }


	

    public void markBaseChanged() {
        baseChanged = true;

        
        
        String oldTitle = frame.getTabTitle(this);
        if (!oldTitle.endsWith("*"))
            frame.setTabTitle(this, oldTitle+"*", frame.getTabTooltip(this));

        
        
        
        if (frame.statusLine.getText().startsWith("Saved database"))
            frame.output(" ");
    }


	

    public void markNonUndoableBaseChanged() {
        nonUndoableChange = true;
        markBaseChanged();
    }


	

    public synchronized void markChangedOrUnChanged() {
        if (undoManager.hasChanged()) {
            if (!baseChanged)
                markBaseChanged();
        }
        else if (baseChanged && !nonUndoableChange) {
            baseChanged = false;
            if (getFile() != null)
                frame.setTabTitle(BasePanel.this, getFile().getName(),
                        getFile().getAbsolutePath());
            else
                frame.setTabTitle(BasePanel.this, Globals.lang("untitled"), null);
        }
    }


	

    
    public void selectSingleEntry(int pos) {
        mainTable.clearSelection();
        mainTable.addRowSelectionInterval(pos, pos);
        mainTable.scrollToCenter(pos, 0);
    }


	

    


    public void setSearchMatcher(SearchMatcher matcher) {
        searchFilterList.setMatcher(matcher);
    }


	

    


	

    public void stopShowingSearchResults() {
        searchFilterList.setMatcher(NoSearchMatcher.INSTANCE);
    }


	

    public void stopShowingGroup() {
        groupFilterList.setMatcher(NoSearchMatcher.INSTANCE);

     }


	

     public BibtexDatabase getDatabase(){
        return database ;
    }


	

    public void preambleEditorClosing() {
        preambleEditor = null;
    }


	

    public void stringsClosing() {
        stringDialog = null;
    }


	

    public void changeType(BibtexEntry entry, BibtexEntryType type) {
      changeType(new BibtexEntry[] {entry}, type);
    }


	

    public void changeType(BibtexEntryType type) {
      BibtexEntry[] bes = mainTable.getSelectedEntries();
      changeType(bes, type);
    }


	

    public void changeType(BibtexEntry[] bes, BibtexEntryType type) {

        if ((bes == null) || (bes.length == 0)) {
            output("First select the entries you wish to change type "+
                   "for.");
            return;
        }
        if (bes.length > 1) {
            int choice = JOptionPane.showConfirmDialog
                (this, "Multiple entries selected. Do you want to change"
                 +"\nthe type of all these to '"+type.getName()+"'?",
                 "Change type", JOptionPane.YES_NO_OPTION,
                 JOptionPane.WARNING_MESSAGE);
            if (choice == JOptionPane.NO_OPTION)
                return;
        }

        NamedCompound ce = new NamedCompound(Globals.lang("change type"));
        for (int i=0; i<bes.length; i++) {
            ce.addEdit(new UndoableChangeType(bes[i],
                                              bes[i].getType(),
                                              type));
            bes[i].setType(type);
        }

        output(Globals.lang("Changed type to")+" '"+type.getName()+"' "
               +Globals.lang("for")+" "+bes.length
               +" "+Globals.lang("entries")+".");
        ce.end();
        undoManager.addEdit(ce);
        markBaseChanged();
        updateEntryEditorIfShowing();
    }


	

    public boolean showDeleteConfirmationDialog(int numberOfEntries) {
        if (Globals.prefs.getBoolean("confirmDelete")) {
            String msg = Globals.lang("Really delete the selected")
                + " " + Globals.lang("entry") + "?",
                title = Globals.lang("Delete entry");
            if (numberOfEntries > 1) {
                msg = Globals.lang("Really delete the selected")
                    + " " + numberOfEntries + " " + Globals.lang("entries") + "?";
                title = Globals.lang("Delete multiple entries");
            }

            CheckBoxMessage cb = new CheckBoxMessage
                (msg, Globals.lang("Disable this confirmation dialog"), false);

            int answer = JOptionPane.showConfirmDialog(frame, cb, title,
                                                       JOptionPane.YES_NO_OPTION,
                                                       JOptionPane.QUESTION_MESSAGE);
            if (cb.isSelected())
                Globals.prefs.putBoolean("confirmDelete", false);
            return (answer == JOptionPane.YES_OPTION);
        } else return true;

    }


	
    
    
    public void autoGenerateKeysBeforeSaving() {
        if (Globals.prefs.getBoolean("generateKeysBeforeSaving")) {
            NamedCompound ce = new NamedCompound(Globals.lang("autogenerate keys"));
            boolean any = false;
            
            for (BibtexEntry bes : database.getEntries()){
                String oldKey = bes.getCiteKey();
                if ((oldKey == null) || (oldKey.equals(""))) {
                    LabelPatternUtil.makeLabel(Globals.prefs.getKeyPattern(), database, bes);
                    ce.addEdit(new UndoableKeyChange(database, bes.getId(), null,
                        (String)bes.getField(BibtexFields.KEY_FIELD)));
                    any = true;
                }
            }
            
            if (any) {
                ce.end();
                undoManager.addEdit(ce);
            }
        }
    }


	
    
    
    public void setPreviewActive(boolean enabled) {
        selectionListener.setPreviewActive(enabled);
    }


	


     


    class  UndoAction  extends BaseAction {
		
        public void action() {
            try {
                String name = undoManager.getUndoPresentationName();
                undoManager.undo();
                markBaseChanged();
                frame.output(name);
            } catch (CannotUndoException ex) {
                frame.output(Globals.lang("Nothing to undo")+".");
            }
            
            
            
            
            markChangedOrUnChanged();
        }



	}

	

     

    class  RedoAction  extends BaseAction {
		
        public void action() {
            try {
                String name = undoManager.getRedoPresentationName();
                undoManager.redo();
                markBaseChanged();
                frame.output(name);
            } catch (CannotRedoException ex) {
                frame.output(Globals.lang("Nothing to redo")+".");
            }
            
            
            
            
            markChangedOrUnChanged();
        }



	}

	

    
    public void lostOwnership(Clipboard clipboard, Transferable contents) {}


	


  public void setEntryEditorEnabled(boolean enabled) {
    if ((showing != null) && (splitPane.getBottomComponent() instanceof EntryEditor)) {
          EntryEditor ed = (EntryEditor)splitPane.getBottomComponent();
          if (ed.isEnabled() != enabled)
            ed.setEnabled(enabled);
    }
  }


	

  public String fileMonitorHandle() { return fileMonitorHandle; }


	

    public void fileUpdated() {
      if (saving)
        return; 
      
      
      updatedExternally = true;

      final ChangeScanner scanner = new ChangeScanner(frame, BasePanel.this);

      
      
      Thread t = new Thread() {
	      public void run() {
		  
		  
		  
		  boolean hasAlready = sidePaneManager.hasComponent(FileUpdatePanel.NAME);
		  if (hasAlready) {
		      sidePaneManager.hideComponent(FileUpdatePanel.NAME);
		      sidePaneManager.unregisterComponent(FileUpdatePanel.NAME);
		  }
		  FileUpdatePanel pan = new FileUpdatePanel(frame, BasePanel.this,
							    sidePaneManager, getFile(), scanner);
		  sidePaneManager.register(FileUpdatePanel.NAME, pan);
		  sidePaneManager.show(FileUpdatePanel.NAME);
		  setUpdatedExternally(false);
		  
	      }
	  };

      
      scanner.changeScan(BasePanel.this.getFile());
      try {
	  scanner.join();
      } catch (InterruptedException e) {
	  e.printStackTrace();
      }

      if (scanner.changesFound()) {
	  SwingUtilities.invokeLater(t);
      } else {
	  setUpdatedExternally(false);
	  
      }
    }


	

      public void fileRemoved() {
        Util.pr("File '"+getFile().getPath()+"' has been deleted.");
      }


	


      public void cleanUp() {
        if (fileMonitorHandle != null)
          Globals.fileUpdateMonitor.removeUpdateListener(fileMonitorHandle);
      }


	

  public void setUpdatedExternally(boolean b) {
    updatedExternally = b;
  }


	

    
    public BibtexEntry[] getSelectedEntries() {
        return mainTable.getSelectedEntries();
    }


	

    
    public File getFile() {
        return metaData.getFile();
    }


	
    
    
    public String getKeysForSelection() {
        StringBuffer result = new StringBuffer();
        String citeKey = "";
        boolean first = true;
        for (BibtexEntry bes : mainTable.getSelected()){
            citeKey = (String) bes.getField(BibtexFields.KEY_FIELD);
            
            if (citeKey == null || citeKey.equals(""))
                continue;
            if (first) {
                result.append(citeKey);
                first = false;
            } else {
                result.append(",").append(citeKey);
            }
        }
        return result.toString();
    }


	

    public GroupSelector getGroupSelector() {
        return frame.groupSelector;
    }


	


    public boolean isUpdatedExternally() {
        return updatedExternally;
    }

	


    public String getFileMonitorHandle() {
        return fileMonitorHandle;
    }

	


    public void setFileMonitorHandle(String fileMonitorHandle) {
        this.fileMonitorHandle = fileMonitorHandle;
    }

	

    public SidePaneManager getSidePaneManager() {
        return sidePaneManager;
    }

	


    public void setNonUndoableChange(boolean nonUndoableChange) {
        this.nonUndoableChange = nonUndoableChange;
    }

	

    public void setBaseChanged(boolean baseChanged) {
        this.baseChanged = baseChanged;
    }

	


    public void setSaving(boolean saving) {
        this.saving = saving;
    }

	
    private ListEventListener<BibtexEntry> groupsHighlightListener;

	

    HashMap<String, AutoCompleter> autoCompleters = new HashMap<String, AutoCompleter>();

	
    public FilterList<BibtexEntry> searchFilterList = null, groupFilterList = null;

	
    
    public HashMap<String, EntryEditor> entryEditors = new HashMap<String, EntryEditor>();

	


    
    public void parseMetaData(HashMap<String, String> meta) {
        metaData = new MetaData(meta,database());

    }

	

    public void setGroupMatcher(Matcher<BibtexEntry> matcher) {
        groupFilterList.setMatcher(matcher);
    }


}
