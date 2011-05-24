package com.horstmann.violet.framework;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.ResourceBundle;

import javax.swing.JFileChooser;
import javax.swing.JOptionPane;  
  
  /**
    * A Save object encapsulates the stream and name of the file that the user selected for saving.
    */
   public interface Save
   {
      /**
       * Gets the output stream corresponding to the user selection.
       * @return the output stream
       */
      OutputStream getOutputStream() throws IOException ;
      /**
       * Gets the name of the file that the user selected.
       * @return the file name, or null if the file dialog is only displayed when the output
       * stream is closed.
       */
      String getName() throws IOException ;
   }