package net.sf.jabref.imports;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

import javax.swing.JMenuItem;
import javax.swing.JOptionPane;

import net.sf.jabref.AbstractWorker;
import net.sf.jabref.BasePanel;
import net.sf.jabref.BibtexDatabase;
import net.sf.jabref.BibtexEntry;
import net.sf.jabref.BibtexEntryType;
import net.sf.jabref.BibtexFields;
import net.sf.jabref.BibtexString;
import net.sf.jabref.DuplicateCheck;
import net.sf.jabref.DuplicateResolverDialog;
import net.sf.jabref.Globals;
import net.sf.jabref.JabRefFrame;
import net.sf.jabref.KeyCollisionException;
import net.sf.jabref.Util;
import net.sf.jabref.gui.ImportInspectionDialog;
import net.sf.jabref.labelPattern.LabelPatternUtil;
import net.sf.jabref.undo.NamedCompound;
import net.sf.jabref.undo.UndoableInsertEntry;
import net.sf.jabref.undo.UndoableRemoveEntry;
import net.sf.jabref.util.Pair;


public class ImportMenuItem extends JMenuItem implements ActionListener {

    JabRefFrame frame;
    boolean openInNew;
    MyWorker worker = null;
    ImportFormat importer;

    public ImportMenuItem(JabRefFrame frame, boolean openInNew) {
        this(frame, openInNew, null);
    }

    public ImportMenuItem(JabRefFrame frame, boolean openInNew, ImportFormat importer) {
        super(importer != null ? importer.getFormatName()
                : Globals.lang("Autodetect format"));
        this.importer = importer;
        this.frame = frame;
        this.openInNew = openInNew;
        addActionListener(this);
    }

    public void actionPerformed(ActionEvent e) {
        worker = new MyWorker();
        worker.init();
        worker.getWorker().run();
        worker.getCallBack().update();
    }
    
    
    public void automatedImport(String filenames[]) {
        
        MyWorker worker = new MyWorker();
        worker.fileOk = true;
        worker.filenames = filenames;
        
        worker.getWorker().run();
        worker.getCallBack().update();
    }
    

    class MyWorker extends AbstractWorker {
        String[] filenames = null, formatName = null;
        ParserResult bibtexResult = null; 
        boolean fileOk = false;

        public void init() {
            filenames = Globals.getMultipleFiles(frame,
                    new File(Globals.prefs.get("workingDirectory")),
                    (importer != null ? importer.getExtensions() : null), true);

            if ((filenames != null) && (filenames.length > 0)) {
                frame.block();
                frame.output(Globals.lang("Starting import"));
                fileOk = true;
                
                Globals.prefs.put("workingDirectory", filenames[0]);
            }
        }

        public void run() {
            if (!fileOk)
                return;

            
			List<Pair<String, ParserResult>> imports = new ArrayList<Pair<String, ParserResult>>();
			for (String filename : filenames) {
				try {
					if (importer != null) {
						
						ParserResult pr = new ParserResult(
							Globals.importFormatReader.importFromFile(importer,
								filename));

						imports.add(new Pair<String, ParserResult>(importer
							.getFormatName(), pr));
					} else {
						
						imports.add(Globals.importFormatReader
							.importUnknownFormat(filename));
					}
				} catch (IOException e) {
					
                    e.printStackTrace();
                }
			}

            
			
            
			
            bibtexResult = mergeImportResults(imports);
        }

        public void update() {
            if (!fileOk)
                return;

            
            
            if (bibtexResult != null) {
                if (!openInNew) {
                    final BasePanel panel = (BasePanel) frame.getTabbedPane().getSelectedComponent();
                    BibtexDatabase toAddTo = panel.database();
                    
                    
                    
                    
                    if (Globals.prefs.getBoolean("useImportInspectionDialog") &&
                            (Globals.prefs.getBoolean("useImportInspectionDialogForSingle")
                                    || (bibtexResult.getDatabase().getEntryCount() > 1))) {
                        ImportInspectionDialog diag = new ImportInspectionDialog(frame, panel,
                                BibtexFields.DEFAULT_INSPECTION_FIELDS,
                                Globals.lang("Import"), openInNew);
                        diag.addEntries(bibtexResult.getDatabase().getEntries());
                        diag.entryListComplete();
                        Util.placeDialog(diag, frame);
                        diag.setVisible(true);
                        diag.toFront();
                    } else {
                        boolean generateKeys = Globals.prefs.getBoolean("generateKeysAfterInspection");
                        NamedCompound ce = new NamedCompound(Globals.lang("Import entries"));
                        
                        for (BibtexEntry entry : bibtexResult.getDatabase().getEntries()){
                            try {
                                
                                boolean keepEntry = true;
                                BibtexEntry duplicate = DuplicateCheck.containsDuplicate(toAddTo, entry);
                                if (duplicate != null) {
                                    int answer = DuplicateResolverDialog.resolveDuplicateInImport
                                            (frame, duplicate, entry);
                                    
                                    if (answer == DuplicateResolverDialog.DO_NOT_IMPORT)
                                        keepEntry = false;
                                    if (answer == DuplicateResolverDialog.IMPORT_AND_DELETE_OLD) {
                                        
                                        toAddTo.removeEntry(duplicate.getId());
                                        ce.addEdit(new UndoableRemoveEntry(toAddTo, duplicate, panel));
                                    }
                                }
                                
                                if (keepEntry) {
                                    toAddTo.insertEntry(entry);
                                    
                                    if (generateKeys) {
                                        LabelPatternUtil.makeLabel(Globals.prefs.getKeyPattern(), toAddTo, entry);
                                        
                                    }
                                    
                                    Util.updateCompletersForEntry(panel.getAutoCompleters(), entry);

                                    ce.addEdit(new UndoableInsertEntry(toAddTo, entry, panel));
                                }
                            } catch (KeyCollisionException e) {
                                e.printStackTrace();
                            }
                        }
                        ce.end();
                        panel.undoManager.addEdit(ce);
                    }

                }

                else {
                    frame.addTab(bibtexResult.getDatabase(), bibtexResult.getFile(),
                            bibtexResult.getMetaData(), Globals.prefs.get("defaultEncoding"), true);
                    frame.output(Globals.lang("Imported entries") + ": " + bibtexResult.getDatabase().getEntryCount());
                }


            } else {
                if (importer == null)
                    frame.output(Globals.lang("Could not find a suitable import format."));
                else
                    JOptionPane.showMessageDialog(frame, Globals.lang("No entries found. Please make sure you are "
								  +"using the correct import filter."), Globals.lang("Import failed"),
					      JOptionPane.ERROR_MESSAGE);
            }
            frame.unblock();
        }
    }

    public static ParserResult mergeImportResults(List<Pair<String, ParserResult>> imports) {
        BibtexDatabase database = new BibtexDatabase();
        ParserResult directParserResult = null;
        boolean anythingUseful = false;

        for (Pair<String, ParserResult> importResult : imports){
            if (importResult == null)
                continue;
            if (importResult.p.equals(ImportFormatReader.BIBTEX_FORMAT)){
        	    
                ParserResult pr = importResult.v;

                anythingUseful = anythingUseful
                        || ((pr.getDatabase().getEntryCount() > 0) || (pr.getDatabase().getStringCount() > 0));
                
                
                if (directParserResult == null) {
                    directParserResult = pr;
                }

                
                for (BibtexEntry entry : pr.getDatabase().getEntries()) {
                    database.insertEntry(entry);
                }
                
                
                for (BibtexString bs : pr.getDatabase().getStringValues()){
                    try {
                        database.addString((BibtexString)bs.clone());
                    } catch (KeyCollisionException e) {
                        
                        
                    }
                }
            } else {
            	
            	ParserResult pr = importResult.v;
				Collection<BibtexEntry> entries = pr.getDatabase().getEntries();

				anythingUseful = anythingUseful | (entries.size() > 0);

				
				Util.setAutomaticFields(entries, Globals.prefs.getBoolean("overwriteOwner"),
                        Globals.prefs.getBoolean("overwriteTimeStamp")); 

                for (BibtexEntry entry : entries){
					database.insertEntry(entry);
				}
			}
        }

        if (!anythingUseful)
            return null;

        if ((imports.size() == 1) && (directParserResult != null)) {
            return directParserResult;
        } else {

            ParserResult pr = new ParserResult(database, new HashMap<String, String>(), new HashMap<String, BibtexEntryType>());
            return pr;

        }
    }

}
