
package net.sf.jabref;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JOptionPane;

import net.sf.jabref.net.URLDownload;



public class UrlDragDrop implements DropTargetListener {

    private static Logger logger = Logger
            .getLogger(UrlDragDrop.class.getName());

    private FieldEditor feditor;

    private EntryEditor editor;

    private JabRefFrame frame;

    public UrlDragDrop(EntryEditor _editor, JabRefFrame _frame,
            FieldEditor _feditor) {
        editor = _editor;
        feditor = _feditor;
        frame = _frame;
    }

    
    public void dragEnter(DropTargetDragEvent dtde) {
    }

    
    public void dragOver(DropTargetDragEvent dtde) {
    }

    
    public void dropActionChanged(DropTargetDragEvent dtde) {
    }

    
    public void dragExit(DropTargetEvent dte) {
    }

    private static class JOptionChoice {

        private String label;

        private int id;

        public JOptionChoice(String _label, int _id) {
            label = _label;
            id = _id;
        }

        public String toString() {
            return label;
        }

        public int getId() {
            return id;
        }

    }

    
    
	public void drop(DropTargetDropEvent dtde) {
        Transferable tsf = dtde.getTransferable();
        dtde.acceptDrop(DnDConstants.ACTION_COPY_OR_MOVE);
        
        DataFlavor dtURL = null;
        try{
            dtURL = new DataFlavor("application/x-java-url; class=java.net.URL");
        }catch (ClassNotFoundException e){
            logger.log(Level.WARNING,
                    "Class not found for DnD... should not happen", e);
        }
        try{
            URL url = (URL) tsf.getTransferData(dtURL);
            JOptionChoice res = (JOptionChoice) JOptionPane
                    .showInputDialog(editor, "", Globals
                            .lang("Select action"),
                            JOptionPane.QUESTION_MESSAGE, null,
                            new JOptionChoice[] {
                                    new JOptionChoice(Globals
                                            .lang("Insert URL"), 0),
                                    new JOptionChoice(Globals
                                            .lang("Download file"), 1) },
                            new JOptionChoice(Globals.lang("Insert URL"), 0));
            switch (res.getId()) {
            
            case 0:
                feditor.setText(url.toString());
                editor.updateField(feditor);
                break;
            
            case 1:
                try{
                    
                    File file = new File(new File(Globals.prefs
                            .get("pdfDirectory")), editor.getEntry()
                            .getField(BibtexFields.KEY_FIELD)
                            + ".pdf");
                    URLDownload udl = new URLDownload(editor, url,
                            file);
                    frame.output(Globals.lang("Downloading..."));
                    udl.download();
                    frame.output(Globals.lang("Download completed"));
                    feditor.setText(file.toURI().toURL().toString());
                    editor.updateField(feditor);

                }catch (IOException ioex){
                    logger.log(Level.SEVERE, "Error while downloading file",
                            ioex);
                    JOptionPane.showMessageDialog(editor, Globals
                            .lang("File download"), Globals
                            .lang("Error while downloading file:"
                                    + ioex.getMessage()),
                            JOptionPane.ERROR_MESSAGE);
                }
                break;
            }
            return;
        }catch (UnsupportedFlavorException nfe){
            
        }catch (IOException ioex){
            logger.log(Level.WARNING, "!should not happen!", ioex);
        }
        
        try{
            

        	List<File> filelist = (List<File>) tsf
                    .getTransferData(DataFlavor.javaFileListFlavor);
            if (filelist.size() > 1){
                JOptionPane
                        .showMessageDialog(editor, Globals
                                .lang("Only one item is supported"), Globals
                                .lang("Drag and Drop Error"),
                                JOptionPane.ERROR_MESSAGE);
                return;
            }
            File fl = filelist.get(0);
            feditor.setText(fl.toURI().toURL().toString());
            editor.updateField(feditor);

        }catch (UnsupportedFlavorException nfe){
            JOptionPane.showMessageDialog(editor, Globals
                    .lang("Operation not supported"), Globals
                    .lang("Drag and Drop Error"), JOptionPane.ERROR_MESSAGE);
            logger.log(Level.WARNING, "Transfer exception", nfe);
        }catch (IOException ioex){
            logger.log(Level.WARNING, "Transfer exception", ioex);
        }

    }

}
