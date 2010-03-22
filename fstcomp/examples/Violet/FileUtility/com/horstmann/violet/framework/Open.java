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
    * An Open object encapsulates the stream and name of the file that the user selected for opening.
    */
   public interface Open
   {
      /**
       * Gets the input stream corresponding to the user selection.
       * @return the input stream
       */
      InputStream getInputStream() throws IOException ;
      /**
       * Gets the name of the file that the user selected.
       * @return the file name
       */
      String getName() throws IOException ;
   }