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

/*
 * This class produces common file open and file save dialogs for normal operation and for Java Web Start.
 * Note that the JNLP service is loaded lazily: the JNLP library need not be present for local execution.
 */
public abstract class FileService
{
    /**
    * Gets a service that is appropriate for the mode in which this program works.
    * @return a service for local dialogs or for Java Web Start
     */
   public static synchronized FileService getInstance(File initialDirectory)
   {
      if (service != null) return service;
      try
      {
         service = new JFileChooserService(initialDirectory);
         return service;
      }
      catch (SecurityException exception)
      {
         // that happens when we run under Web Start
      }
/*      try
      {
         // we load this lazily so that the JAR can load without WebStart
         service = (FileService) Class.forName("violet.com.horstmann.violet.framework.JNLPFileService").newInstance();
         return service;
      }
      catch (Exception exception)
      {
         exception.printStackTrace();
      }
*/
      return null;
   }

   /**
    * Tests whether the service is provided by WebStart
    * @return true if this service is provided by WebStart
    */
   public abstract boolean isWebStart();

   /**
    * Gets an Open object that encapsulates the stream and name of the file that the user selected
    * @param defaultDirectory the default directory for the file chooser
    * @param defaultFile the default file for the file chooser
    * @param extensions the extension filter
    * @return the Open object for the selected file
    * @throws IOException
    */
   public abstract Open open(String defaultDirectory, String defaultFile, ExtensionFilter extensions) throws IOException;
   /**
    * Gets a Save object that encapsulates the stream and name of the file that the user selected (or will
    * select)
    * @param defaultDirectory the default directory for the file chooser
    * @param defaultFile the default file for the file chooser
    * @param extensions the extension filter
    * @param removeExtension the extension to remove from the default file name
    * @param addExtension the extension to add to the file name
    * @return the Save object for the selected file
    * @throws IOException
    */
   public abstract Save save(String defaultDirectory, String defaultFile, ExtensionFilter extensions,
      String removeExtension, String addExtension) throws IOException;

   private static boolean webStart = false;
   private static FileService service;

   /**
   Edits the file path so that it ends in the desired
   extension.
   @param original_ the file to use as a starting point
   @param toBeRemoved the extension that is to be
   removed before adding the desired extension. Use
   null if nothing needs to be removed.
   @param desired the desired extension (e.g. ".png"),
   or a | separated list of extensions
   @return original_ if it already has the desired
   extension, or a new file with the edited file path
   */

 public static String editExtension(String original_,
        String toBeRemoved, String desired)
   {
    if (original_ == null) return null;
    int n = desired.indexOf('|');
    if (n >= 0) desired = desired.substring(0, n);
      String path = original_;
    if (!path.toLowerCase().endsWith(desired.toLowerCase()))
      {
        if (toBeRemoved != null && path.toLowerCase().endsWith(
                toBeRemoved.toLowerCase()))
            path = path.substring(0, path.length() - toBeRemoved.length());
         path = path + desired;
      }
    return path;
   }
}
