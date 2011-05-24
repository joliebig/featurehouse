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
    * This class implements a FileService with a JFileChooser
    */
   class JFileChooserService extends FileService
   {
      public JFileChooserService(File initialDirectory)
      {
         fileChooser = new JFileChooser();
         fileChooser.setCurrentDirectory(initialDirectory);
      }

      public Open open(String defaultDirectory, String defaultFile,
         ExtensionFilter filter) throws FileNotFoundException
      {
         fileChooser.resetChoosableFileFilters();
         fileChooser.setFileFilter(filter);
         if (defaultDirectory != null)
            fileChooser.setCurrentDirectory(new File(defaultDirectory));
         if (defaultFile == null)
            fileChooser.setSelectedFile(null);
         else
            fileChooser.setSelectedFile(new File(defaultFile));
         int response = fileChooser.showOpenDialog(null);
         if (response == JFileChooser.APPROVE_OPTION)
            file = fileChooser.getSelectedFile();
         else
            file = null;
         return new JOpen(this);
      }

      public Save save(String defaultDirectory, String defaultFile,
         ExtensionFilter filter, String removeExtension, String addExtension) throws FileNotFoundException
      {
         fileChooser.resetChoosableFileFilters();
         fileChooser.setFileFilter(filter);
         if (defaultDirectory == null)
            fileChooser.setCurrentDirectory(new File("."));
         else
            fileChooser.setCurrentDirectory(new File(defaultDirectory));
         if (defaultFile != null)
         {
            File f = new File(editExtension(defaultFile, removeExtension, addExtension));
            fileChooser.setSelectedFile(f);
         }
         else
            fileChooser.setSelectedFile(new File(""));
         int response = fileChooser.showSaveDialog(null);
         if (response == JFileChooser.APPROVE_OPTION)
         {
            File f = fileChooser.getSelectedFile();
            if (addExtension != null && f.getName().indexOf(".") < 0) // no extension supplied
               f = new File(f.getPath() + addExtension);
            if (!f.exists()) return new JSave(f);

            ResourceBundle editorResources =
               ResourceBundle.getBundle("com.horstmann.violet.framework.EditorStrings");
            int result = JOptionPane.showConfirmDialog(
                  null,
                  editorResources.getString("dialog.overwrite"),
                  null,
                  JOptionPane.YES_NO_OPTION);
            if (result == JOptionPane.YES_OPTION)
               return new JSave(f);
         }

         return new JSave(null);
      }

      public boolean isWebStart() { return false; }

      private JFileChooser fileChooser;
      File file;
   }