
package net.sf.jabref; 

import java.awt.Component; 
import java.awt.datatransfer.DataFlavor; 
import java.awt.datatransfer.Transferable; 
import java.awt.datatransfer.UnsupportedFlavorException; 
import java.awt.dnd.DnDConstants; 
import java.awt.dnd.DropTargetDragEvent; 
import java.awt.dnd.DropTargetDropEvent; 
import java.awt.dnd.DropTargetEvent; 
import java.awt.dnd.DropTargetListener; 
import java.awt.event.ActionEvent; 
import java.io.IOException; 
import java.net.URL; 
import java.util.logging.Level; 
import java.util.logging.Logger; 

import javax.swing.JOptionPane; 

import net.sf.jabref.EntryEditor.StoreFieldAction; 
import java.awt.dnd.*; 



public  class  SimpleUrlDragDrop implements  DropTargetListener {
	

    private static Logger logger = Logger.getLogger(SimpleUrlDragDrop.class
            .getName());

	

    private FieldEditor editor;

	

    private StoreFieldAction storeFieldAction;

	

    public SimpleUrlDragDrop(FieldEditor _editor,
            StoreFieldAction _storeFieldAction) {
        editor = _editor;
        storeFieldAction = _storeFieldAction;
    }


	

    
    public void dragEnter(DropTargetDragEvent dtde) {
    }


	

    
    public void dragOver(DropTargetDragEvent dtde) {
    }


	

    
    public void dropActionChanged(DropTargetDragEvent dtde) {
    }


	

    
    public void dragExit(DropTargetEvent dte) {
    }


	

      class  JOptionChoice {
		

        

		

        

		

        


		

        


		

        



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
            
            editor.setText(url.toString());
            storeFieldAction.actionPerformed(new ActionEvent(editor, 0, ""));
            return;
        }catch (UnsupportedFlavorException nfe){
            
            JOptionPane.showMessageDialog((Component) editor, Globals
                    .lang("Operation not supported"), Globals
                    .lang("Drag and Drop Error"), JOptionPane.ERROR_MESSAGE);
            logger.log(Level.WARNING, "Transfer exception", nfe);
        }catch (IOException ioex){
            logger.log(Level.WARNING, "!should not happen!", ioex);
        }
    }



}
