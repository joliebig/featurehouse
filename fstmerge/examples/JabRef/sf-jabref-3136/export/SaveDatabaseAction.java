package net.sf.jabref.export;

import com.jgoodies.forms.builder.DefaultFormBuilder;
import com.jgoodies.forms.layout.FormLayout;
import net.sf.jabref.*;
import net.sf.jabref.gui.FileDialogs;
import net.sf.jabref.collab.ChangeScanner;

import javax.swing.*;
import java.io.File;
import java.io.IOException;
import java.nio.charset.UnsupportedCharsetException;
import java.util.Vector;


public class SaveDatabaseAction extends AbstractWorker {
    private BasePanel panel;
    private JabRefFrame frame;
    private boolean success = false, cancelled = false, fileLockedError = false;

    public SaveDatabaseAction(BasePanel panel) {

        this.panel = panel;
        this.frame = panel.frame();
    }


    public void init() throws Throwable {
        success = false;
        cancelled = false;
        fileLockedError = false;
        if (panel.getFile() == null)
            saveAs();
        else {

            
            if (panel.isUpdatedExternally() || Globals.fileUpdateMonitor.hasBeenModified(panel.getFileMonitorHandle())) {

                String[] opts = new String[]{Globals.lang("Review changes"), Globals.lang("Save"),
                        Globals.lang("Cancel")};
                int answer = JOptionPane.showOptionDialog(panel.frame(), Globals.lang("File has been updated externally. "
                        + "What do you want to do?"), Globals.lang("File updated externally"),
                        JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE,
                        null, opts, opts[0]);
                

                if (answer == JOptionPane.CANCEL_OPTION) {
                    cancelled = true;
                    return;
                }
                else if (answer == JOptionPane.YES_OPTION) {
                    

                    cancelled = true;

                    (new Thread(new Runnable() {
                        public void run() {

                            if (!Util.waitForFileLock(panel.getFile(), 10)) {
                                
                                System.err.println("File locked, this will be trouble.");
                            }

                            ChangeScanner scanner = new ChangeScanner(panel.frame(), panel);
                            scanner.changeScan(panel.getFile());
                            try {
                                scanner.join();
                            } catch (InterruptedException e) {
                                e.printStackTrace();
                            }
                            if (scanner.changesFound()) {
                                scanner.displayResult(new ChangeScanner.DisplayResultCallback() {
                                    public void scanResultsResolved(boolean resolved) {
                                        if (!resolved) {
                                            cancelled = true;
                                        } else {
                                            panel.setUpdatedExternally(false);
                                            SwingUtilities.invokeLater(new Runnable() {
                                                public void run() {
                                                    panel.getSidePaneManager().hide("fileUpdate");
                                                }
                                            });
                                        }
                                    }
                                });
                            }
                        }
                    })).start();

                    return;
                }
                else { 
                    
                    Vector<String> pd = panel.metaData().getData(Globals.PROTECTED_FLAG_META);
                    boolean databaseProtectionFlag = (pd != null) && Boolean.parseBoolean(pd.get(0));
                    if (databaseProtectionFlag) {
                        JOptionPane.showMessageDialog(frame, Globals.lang("Database is protected. Cannot save until external changes have been reviewed."),
                                Globals.lang("Protected database"), JOptionPane.ERROR_MESSAGE);
                        cancelled = true;
                    }
                    else {
                        panel.setUpdatedExternally(false);
                        panel.getSidePaneManager().hide("fileUpdate");
                    }
                }
            }

            panel.frame().output(Globals.lang("Saving database") + "...");
            panel.setSaving(true);
        }
    }

    public void update() {
        if (success) {
            
            frame.setTabTitle(panel, panel.getFile().getName(),
                    panel.getFile().getAbsolutePath());
            frame.output(Globals.lang("Saved database") + " '"
                    + panel.getFile().getPath() + "'.");
        } else if (!cancelled) {
            if (fileLockedError) {
                
                frame.output(Globals.lang("Could not save, file locked by another JabRef instance."));
            } else
                frame.output(Globals.lang("Save failed"));
        }
    }

    public void run() {
        if (cancelled || (panel.getFile() == null)) {
            return;
        }

        try {

            
            panel.storeCurrentEdit();

            
            
            panel.autoGenerateKeysBeforeSaving();

            if (!Util.waitForFileLock(panel.getFile(), 10)) {
                success = false;
                fileLockedError = true;
            }
            else {
                
                success = saveDatabase(panel.getFile(), false, panel.getEncoding());

                
                
                

                try {
                    Globals.fileUpdateMonitor.updateTimeStamp(panel.getFileMonitorHandle());
                } catch (IllegalArgumentException ex) {
                    
                    
                    
                }
            }
            panel.setSaving(false);
            if (success) {
                panel.undoManager.markUnchanged();

                if (!AutoSaveManager.deleteAutoSaveFile(panel)) {
                    System.out.println("Deletion of autosave file failed");
                } else
                    System.out.println("Deleted autosave file (if it existed)");
                
                
                
                panel.setNonUndoableChange(false);
                panel.setBaseChanged(false);
                panel.setUpdatedExternally(false);
            }
        } catch (SaveException ex2) {
            if (ex2 == SaveException.FILE_LOCKED) {
                success =false;
                fileLockedError = true;
                return;
            }
            ex2.printStackTrace();
        }
    }

    private boolean saveDatabase(File file, boolean selectedOnly, String encoding) throws SaveException {
        SaveSession session;
        frame.block();
        try {
            if (!selectedOnly)
                session = FileActions.saveDatabase(panel.database(), panel.metaData(), file,
                        Globals.prefs, false, false, encoding, false);
            else
                session = FileActions.savePartOfDatabase(panel.database(), panel.metaData(), file,
                        Globals.prefs, panel.getSelectedEntries(), encoding);

        } catch (UnsupportedCharsetException ex2) {
            JOptionPane.showMessageDialog(frame, Globals.lang("Could not save file. "
                    + "Character encoding '%0' is not supported.", encoding),
                    Globals.lang("Save database"), JOptionPane.ERROR_MESSAGE);
            throw new SaveException("rt");
        } catch (SaveException ex) {
            if (ex == SaveException.FILE_LOCKED) {
                throw ex;
            }
            if (ex.specificEntry()) {
                
                
                int row = panel.mainTable.findEntry(ex.getEntry()),
                        topShow = Math.max(0, row - 3);
                panel.mainTable.setRowSelectionInterval(row, row);
                panel.mainTable.scrollTo(topShow);
                panel.showEntry(ex.getEntry());
            } else ex.printStackTrace();

            JOptionPane.showMessageDialog
                    (frame, Globals.lang("Could not save file")
                            + ".\n" + ex.getMessage(),
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
                    new String[]{Globals.lang("Save"), tryDiff, Globals.lang("Cancel")}, tryDiff);

            if (answer == JOptionPane.NO_OPTION) {
                
                Object choice = JOptionPane.showInputDialog(frame, Globals.lang("Select encoding"), Globals.lang("Save database"),
                        JOptionPane.QUESTION_MESSAGE, null, Globals.ENCODINGS, encoding);
                if (choice != null) {
                    String newEncoding = (String) choice;
                    return saveDatabase(file, selectedOnly, newEncoding);
                } else
                    commit = false;
            } else if (answer == JOptionPane.CANCEL_OPTION)
                commit = false;


        }

        try {
            if (commit) {
                session.commit();
                panel.setEncoding(encoding); 
            } else
                session.cancel();
        } catch (IOException e) {
            e.printStackTrace();
        }

        return commit;
    }

    
    public void runCommand() throws Throwable {
        
        Worker wrk = getWorker();
        
        
        
        CallBack clb = getCallBack();

        init(); 
        

        
        
        
        wrk.run(); 
        
        
        clb.update(); 

    }

    public void save() throws Throwable {
        runCommand();
    }

    
    public void saveAs() throws Throwable {
        String chosenFile = null;
        File f = null;
        while (f == null) {
            chosenFile = FileDialogs.getNewFile(frame, new File(Globals.prefs.get("workingDirectory")), ".bib",
                    JFileChooser.SAVE_DIALOG, false, null);
            if (chosenFile == null) {
                cancelled = true;
                return; 
            }
            f = new File(chosenFile);
            
            if (f.exists() && (JOptionPane.showConfirmDialog
                    (frame, "'" + f.getName() + "' " + Globals.lang("exists. Overwrite file?"),
                            Globals.lang("Save database"), JOptionPane.OK_CANCEL_OPTION)
                    != JOptionPane.OK_OPTION)) {
                f = null;
            }
        }

        if (chosenFile != null) {
            File oldFile = panel.metaData().getFile();
            panel.metaData().setFile(f);
            Globals.prefs.put("workingDirectory", f.getParent());
            runCommand();
            
            if (!success) {
                panel.metaData().setFile(oldFile);
                return;
            }
            
            try {
                panel.setFileMonitorHandle(Globals.fileUpdateMonitor.addUpdateListener(panel, panel.getFile()));
            } catch (IOException ex) {
                ex.printStackTrace();
            }
            frame.getFileHistory().newFile(panel.metaData().getFile().getPath());
        }

    }

    
    public boolean isSuccess() {
        return success;
    }

    
    public boolean isCancelled() {
        return cancelled;
    }
}
